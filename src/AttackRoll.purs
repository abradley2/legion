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

import D8 (D8(..), rollD8)
import Data.Array (elem, (..), filter)
import Data.Enum (downFromIncluding)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (sequence)
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

rollAttacks :: Variant -> (Result -> Maybe Value) -> Int -> Effect (Array (Maybe Value))
rollAttacks variant resolveSurge count =
  sequence
    $ (resolveSurge <<< toResult variant <$> rollD8)
    <$ filter ((/=) 0) (0 .. count)
