module D8
  ( D8(..)
  , rollD8
  ) where

import Prelude
import Data.Array (elemIndex, (!!))
import Data.Enum (toEnumWithDefaults, fromEnum, class BoundedEnum, class Enum, defaultCardinality)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Random (randomInt)

data D8
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight

d8 :: Array D8
d8 = [ One, Two, Three, Four, Five, Six, Seven, Eight ]

derive instance genericD8 :: Generic D8 _

instance showD8 :: Show D8 where
  show = genericShow

derive instance eqD8 :: Eq D8

derive instance ordD8 :: Ord D8

instance enumD8 :: Enum D8 where
  succ v = elemIndex v d8 >>= (\idx -> d8 !! (idx + 1))
  pred v = elemIndex v d8 >>= (\idx -> d8 !! (idx - 1))

instance boundedD8 :: Bounded D8 where
  bottom = One
  top = Eight

instance boundedEnumD8 :: BoundedEnum D8 where
  cardinality = defaultCardinality
  fromEnum One = 1
  fromEnum Two = 2
  fromEnum Three = 3
  fromEnum Four = 4
  fromEnum Five = 5
  fromEnum Six = 6
  fromEnum Seven = 7
  fromEnum Eight = 8
  toEnum v = d8 !! (v - 1)

rollD8 ∷ Effect D8
rollD8 = do
  intVal <- randomInt (fromEnum (bottom :: D8)) (fromEnum (top :: D8))
  pure $ toEnumWithDefaults (bottom :: D8) (top :: D8) intVal
