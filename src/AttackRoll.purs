module AttackRoll
  ( Result(..)
  , Variant(..)
  , Mods
  , Config
  , rollAttacks
  , toResult
  ) where

import Prelude
import Control.Monad.State.Trans (StateT, evalStateT, state)
import Control.Monad.Trans.Class (lift)
import D8 (D8(..), rollD8)
import Data.Array (elem, replicate)
import Data.Enum (downFromIncluding)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)

data Result
  = Hit
  | Miss
  | Crit
  | Surge

derive instance genericResult :: Generic Result _

instance showResult :: Show Result where
  show = genericShow

applySurge :: Maybe Result -> Result -> Result
applySurge val = case _ of
  Surge -> fromMaybe Surge val
  result -> result

data Variant
  = White
  | Red
  | Black

derive instance genericVariant :: Generic Variant _

derive instance eqVariant :: Eq Variant

instance showVariant :: Show Variant where
  show = genericShow

toResult :: Variant -> D8 -> Result
toResult White d8
  | elem d8 $ downFromIncluding Five = Miss
  | elem d8 $ downFromIncluding Six = Surge
  | elem d8 $ downFromIncluding Seven = Hit
  | otherwise = Crit

toResult Black d8
  | elem d8 $ downFromIncluding Three = Miss
  | elem d8 $ downFromIncluding Four = Surge
  | elem d8 $ downFromIncluding Seven = Hit
  | otherwise = Crit

toResult Red d8
  | elem d8 $ downFromIncluding One = Miss
  | elem d8 $ downFromIncluding Two = Surge
  | elem d8 $ downFromIncluding Seven = Hit
  | otherwise = Crit

type Mods
  = { rerolls :: Int
    , surgeTokens :: Int
    }

type Config
  = { variant :: Variant
    , surge :: Maybe Result
    }

resolveMods :: Config -> List Result -> StateT Mods Effect (List Result)
resolveMods _ Nil = pure Nil

-- leave hits alone
resolveMods config (Cons Hit attacks) =
  Cons Hit
    <$> resolveMods config attacks

-- leave crits alone
resolveMods config (Cons Crit attacks) =
  Cons Crit
    <$> resolveMods config attacks

-- check if we can re-roll misses
resolveMods config (Cons Miss attacks) = do
  reroll <- lift $ applySurge config.surge <$> toResult config.variant <$> rollD8
  next <- resolveMods config attacks
  state \mods ->
    if mods.rerolls > 0 then
      let
        nextMods = (mods { rerolls = mods.rerolls - 1 })
      in
        -- we still need to check if we can spend a surge token on the reroll
        case reroll of
          Surge ->
            if nextMods.surgeTokens > 0 then
              Tuple (Cons Hit next) (nextMods { surgeTokens = nextMods.surgeTokens - 1 })
            else
              Tuple (Cons Miss next) nextMods
          _ -> Tuple (Cons reroll next) (mods { rerolls = mods.rerolls - 1 })
    else
      Tuple (Cons Miss next) mods

-- check if we can spend surge tokens to convert surges
resolveMods config (Cons Surge attacks) = do
  next <- resolveMods config attacks
  state \mods -> do
    if mods.surgeTokens > 0 then
      Tuple (Cons Hit next) (mods { surgeTokens = mods.surgeTokens - 1 })
    else
      Tuple (Cons Miss next) mods

rollAttacks ∷ Config → Mods → Int → Effect (List Result)
rollAttacks config mods count =
  replicate count unit
    # List.fromFoldable
    # map (const rollD8 >>> map (toResult config.variant) >>> map (applySurge config.surge))
    # sequence
    # map (resolveMods config)
    >>= \s -> evalStateT s mods
