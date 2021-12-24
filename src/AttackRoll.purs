module AttackRoll
  ( Result(..)
  , Variant(..)
  , ignoreSurge
  , rollAttacks
  , surgeCrit
  , surgeHit
  , toResult
  ) where

import Prelude

import Control.Monad.State.Trans (StateT, evalStateT, state)
import Control.Monad.Trans.Class (lift)
import D8 (D8(..), rollD8)
import Data.Array (elem, (..), replicate)
import Data.Bifunctor (lmap)
import Data.Enum (downFromIncluding)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
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

applySurge :: Result -> Result -> Result
applySurge val = case _ of
  Surge -> val
  result -> result

surgeHit :: Result -> Result
surgeHit = applySurge Hit

surgeCrit :: Result -> Result
surgeCrit = applySurge Crit

ignoreSurge :: Result -> Result
ignoreSurge = applySurge Surge

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

resolveAttackMods :: Variant -> (Result -> Result) -> List Result -> StateT AttackMods Effect (List Result)
resolveAttackMods _ _ Nil = pure Nil

-- leave hits alone
resolveAttackMods variant resolveSurge (Cons Hit attacks) =
  Cons Hit
    <$> resolveAttackMods variant resolveSurge attacks

-- leave crits alone
resolveAttackMods variant resolveSurge (Cons Crit attacks) =
  Cons Crit
    <$> resolveAttackMods variant resolveSurge attacks

-- check if we can re-roll misses
resolveAttackMods variant resolveSurge (Cons Miss attacks) = do
  reroll <- lift $ resolveSurge <$> toResult variant <$> rollD8
  resolveAttackMods variant resolveSurge attacks
    >>= \next ->
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
resolveAttackMods variant resolveSurge (Cons Surge attacks) = do
  let
    withSurge = resolveSurge Surge
  next <- resolveAttackMods variant resolveSurge attacks
  case withSurge of
    Surge ->
      state \mods -> do
        if mods.surgeTokens > 0 then
          Tuple (Cons Hit next) (mods { surgeTokens = mods.surgeTokens - 1 })
        else
          Tuple (Cons Miss next) mods
    other -> pure $ Cons other next

rollAttacks ∷ Variant → (Result → Result) → AttackMods → Int → Effect (Effect (List Result))
rollAttacks variant resolveSurge attackMods count =
  map (flip evalStateT attackMods <<< resolveAttackMods variant resolveSurge) $ sequence
    $ toResult variant
    <$> rollD8
    <$ List.fromFoldable (replicate count 0)
