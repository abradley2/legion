module DefenseRoll
  ( Result(..)
  , Variant(..)
  , Config
  , Mods
  , Error
  , applySurge
  , rollBlocks
  , toResult
  ) where

import Prelude
import AttackRoll as AttackRoll
import Control.Monad.Except.Trans (ExceptT, lift, throwError)
import Control.Monad.State.Trans (StateT, state)
import D6 (D6(..), rollD6)
import Data.Array (elem)
import Data.Enum (downFromIncluding)
import Data.List (List(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)

data Error
  = InvalidAttack String

type Mods
  = { dodgeTokens :: Int
    , coverValue :: Int
    , surgeTokens :: Int
    }

type Config
  = { variant :: Variant
    , surge :: Maybe Result
    }

rollBlocks :: Config -> List (AttackRoll.Result) -> ExceptT Error (StateT Mods Effect) (List Result)
rollBlocks _ Nil = pure Nil

rollBlocks _ (Cons AttackRoll.Surge _) = throwError (InvalidAttack "Unresolved Surge")

rollBlocks config (Cons AttackRoll.Miss attacks) = rollBlocks config attacks

rollBlocks config (Cons AttackRoll.Hit attacks) = do
  result <- lift $ lift $ applySurge config.surge <$> toResult config.variant <$> rollD6
  next <- rollBlocks config attacks
  state
    ( \mods -> case result of
        Wound ->
          if mods.coverValue > 0 then
            Tuple (Cons Block next) (mods { coverValue = mods.coverValue - 1 })
          else if mods.dodgeTokens > 0 then
            Tuple (Cons Block next) (mods { dodgeTokens = mods.dodgeTokens - 1 })
          else
            Tuple (Cons Wound next) mods
        Surge ->
          if mods.surgeTokens > 0 then
            Tuple (Cons Block next) (mods { surgeTokens = mods.surgeTokens - 1 })
          else
            Tuple (Cons Wound next) mods
        Block -> Tuple (Cons Block next) mods
    )

rollBlocks surge (Cons AttackRoll.Crit attacks) = (Cons Wound) <$> rollBlocks surge attacks

data Variant
  = Red
  | White

derive instance eqVariant :: Eq Variant

data Result
  = Block
  | Wound
  | Surge

derive instance eqResult :: Eq Result

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
