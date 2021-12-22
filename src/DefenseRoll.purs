module DefenseRoll where

import Prelude
import D6 (D6(..), rollD6)
import Data.Array (elem)
import Data.Enum (downFromIncluding)
import Data.Maybe (Maybe(..))

data Variant
  = Red
  | White

data Value
  = Block

data Result
  = Result (Maybe Value)
  | Surge

applySurge :: Maybe Value -> Result -> Maybe Value
applySurge val = case _ of
  Result res -> res
  Surge -> val

ignoreSurge :: Result -> Maybe Value
ignoreSurge = applySurge Nothing

surgeBlock :: Result -> Maybe Value
surgeBlock = applySurge $ Just Block

toResult :: Variant -> D6 -> Result
toResult White d6
  | elem d6 $ downFromIncluding Four = Result Nothing
  | elem d6 $ downFromIncluding Five = Surge
  | otherwise = Result $ Just Block

toResult Red d6
  | elem d6 $ downFromIncluding Two = Result Nothing
  | elem d6 $ downFromIncluding Three = Surge
  | otherwise = Result $ Just Block
