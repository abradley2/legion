module AttackRoll
  ( Result(..)
  , Variant(..)
  , AttackMods
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

type AttackMods
  = { rerolls :: Int
    , surgeTokens :: Int
    }

resolveAttackMods :: Variant -> (Maybe Result) -> List Result -> StateT AttackMods Effect (List Result)
resolveAttackMods _ _ Nil = pure Nil

-- leave hits alone
resolveAttackMods variant surge (Cons Hit attacks) =
  Cons Hit
    <$> resolveAttackMods variant surge attacks

-- leave crits alone
resolveAttackMods variant surge (Cons Crit attacks) =
  Cons Crit
    <$> resolveAttackMods variant surge attacks

-- check if we can re-roll misses
resolveAttackMods variant surge (Cons Miss attacks) = do
  reroll <- lift $ applySurge surge <$> toResult variant <$> rollD8
  next <- resolveAttackMods variant surge attacks
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
resolveAttackMods variant surge (Cons Surge attacks) = do
  next <- resolveAttackMods variant surge attacks
  case applySurge surge Surge of
    Surge ->
      state \mods -> do
        if mods.surgeTokens > 0 then
          Tuple (Cons Hit next) (mods { surgeTokens = mods.surgeTokens - 1 })
        else
          Tuple (Cons Miss next) mods
    other -> pure $ Cons other next

rollAttacks ∷ Variant → (Maybe Result) → AttackMods → Int → Effect (List Result)
rollAttacks variant surge attackMods count =
  replicate count ""
    # List.fromFoldable
    # map (const rollD8 >>> map (toResult variant))
    # sequence
    # map (resolveAttackMods variant surge)
    >>= \s -> evalStateT s attackMods
