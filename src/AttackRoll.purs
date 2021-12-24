module AttackRoll
  ( Result(..)
  , Value(..)
  , Variant(..)
  , ignoreSurge
  , rollAttacks
  , surgeCrit
  , surgeHit
  , toResult
  ) where

import Prelude
import Control.Monad.State.Trans (StateT, state)
import Control.Monad.Trans.Class (lift)
import D8 (D8(..), rollD8)
import Data.Array (elem, (..), replicate)
import Data.Enum (downFromIncluding)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)

data Value
  = Hit
  | Crit

derive instance genericValue :: Generic Value _

instance showValue :: Show Value where
  show = genericShow

data Result
  = Result (Maybe Value)
  | Surge

applySurge :: Maybe Value -> Result -> Maybe Value
applySurge val = case _ of
  Result res -> res
  Surge -> val

surgeHit :: Result -> Maybe Value
surgeHit = applySurge $ Just Hit

surgeCrit :: Result -> Maybe Value
surgeCrit = applySurge $ Just Crit

ignoreSurge :: Result -> Maybe Value
ignoreSurge = applySurge Nothing

data Variant
  = White
  | Red
  | Black

toResult :: Variant -> D8 -> Result
toResult White d8
  | elem d8 $ downFromIncluding Five = Result Nothing
  | elem d8 $ downFromIncluding Six = Surge
  | elem d8 $ downFromIncluding Seven = Result (Just Hit)
  | otherwise = Result (Just Crit)

toResult Black d8
  | elem d8 $ downFromIncluding Three = Result Nothing
  | elem d8 $ downFromIncluding Four = Surge
  | elem d8 $ downFromIncluding Seven = Result (Just Hit)
  | otherwise = Result (Just Crit)

toResult Red d8
  | elem d8 $ downFromIncluding One = Result Nothing
  | elem d8 $ downFromIncluding Two = Surge
  | elem d8 $ downFromIncluding Seven = Result (Just Hit)
  | otherwise = Result (Just Crit)

type AttackMods
  = { rerolls :: Int
    , surgeTokens :: Int
    }

resolveAttackMods :: Variant -> (Result -> Maybe Value) -> List Result -> StateT AttackMods Effect (List (Maybe Value))
resolveAttackMods _ _ Nil = pure Nil

-- leave hits and crits alone
resolveAttackMods variant resolveSurge (Cons (Result (Just hitOrCrit)) attacks) =
  Cons (Just hitOrCrit)
    <$> resolveAttackMods variant resolveSurge attacks

-- check if we can re-roll misses
resolveAttackMods variant resolveSurge (Cons (Result Nothing) attacks) = do
  reroll <- lift $ resolveSurge <$> toResult variant <$> rollD8
  resolveAttackMods variant resolveSurge attacks
    >>= \next ->
        state \mods ->
          if mods.rerolls > 0 then
            Tuple (Cons reroll next) (mods { rerolls = mods.rerolls - 1 })
          else
            Tuple (Cons Nothing next) mods

-- check if we can spend surge tokens to convert surges
resolveAttackMods variant resolveSurge (Cons Surge attacks) = do
  case resolveSurge Surge of
    Just hitOrCrit -> Cons (Just hitOrCrit) <$> resolveAttackMods variant resolveSurge attacks
    Nothing ->
      resolveAttackMods variant resolveSurge attacks
        >>= \next ->
            state \mods -> do
              if mods.surgeTokens > 0 then
                Tuple (Cons (Just Hit) next) (mods { surgeTokens = mods.surgeTokens - 1 })
              else
                Tuple (Cons Nothing next) mods

rollAttacks :: Variant -> (Result -> Maybe Value) -> Int -> Effect (List (Maybe Value))
rollAttacks variant resolveSurge count =
  sequence
    $ (resolveSurge <<< toResult variant <$> rollD8)
    <$ List.fromFoldable (replicate count 0)
