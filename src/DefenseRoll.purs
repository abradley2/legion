module DefenseRoll
  ( Result(..)
  , Variant(..)
  , applySurge
  , rollDefense
  , toResult
  ) where

import Prelude
import D6 (D6(..), rollD6)
import Data.Array (elem)
import Data.Enum (downFromIncluding)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)

data Variant
  = Red
  | White

data Result
  = Block
  | Wound
  | Surge

applySurge :: Maybe Result -> Result -> Result
applySurge val = case _ of
  Surge -> fromMaybe Surge val
  result -> result

toResult :: Variant -> D6 -> Result
toResult White d6
  | elem d6 $ downFromIncluding Four = Wound
  | elem d6 $ downFromIncluding Five = Surge
  | otherwise = Block

toResult Red d6
  | elem d6 $ downFromIncluding Two = Wound
  | elem d6 $ downFromIncluding Three = Surge
  | otherwise = Block

rollDefense :: Variant -> Maybe Result -> Effect Result
rollDefense variant surge = applySurge surge <$> toResult variant <$> rollD6
