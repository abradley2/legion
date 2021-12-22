module D6
  ( D6(..)
  , rollD6
  ) where

import Prelude
import Data.Array (elemIndex, (!!))
import Data.Enum (fromEnum, toEnumWithDefaults, class BoundedEnum, class Enum, defaultCardinality)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Random (randomInt)

data D6
  = One
  | Two
  | Three
  | Four
  | Five
  | Six

d6 :: Array D6
d6 = [ One, Two, Three, Four, Five, Six ]

derive instance genericD8 :: Generic D6 _

instance showD6 :: Show D6 where
  show = genericShow

derive instance eqD6 :: Eq D6

derive instance ordD6 :: Ord D6

instance enumD6 :: Enum D6 where
  succ v = elemIndex v d6 >>= (\idx -> d6 !! (idx + 1))
  pred v = elemIndex v d6 >>= (\idx -> d6 !! (idx - 1))

instance boundedD6 :: Bounded D6 where
  bottom = One
  top = Six

instance boundedEnumD6 :: BoundedEnum D6 where
  cardinality = defaultCardinality
  fromEnum One = 1
  fromEnum Two = 2
  fromEnum Three = 3
  fromEnum Four = 4
  fromEnum Five = 5
  fromEnum Six = 6
  toEnum v = d6 !! (v - 1)

rollD6 ∷ Effect D6
rollD6 = do
  intVal <- randomInt (fromEnum first) (fromEnum last)
  pure $ toEnumWithDefaults first last intVal
  where
  first :: D6
  first = bottom

  last :: D6
  last = top
