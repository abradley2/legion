module Resolve where

import Prelude
import AttackRoll (rollAttacks)
import AttackRoll as AttackRoll
import Control.Monad.State (State, evalState, state)
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DefenseRoll (rollDefense)
import DefenseRoll as DefenseRoll
import Effect (Effect)

type Config
  = { attackConfig :: AttackRoll.Config
    , attackMods :: AttackRoll.Mods
    , attackCount :: Int
    , defense :: Maybe { defenseConfig :: DefenseRoll.Config, defenseMods :: DefenseRoll.Mods }
    }

resolveAttacks ∷ Config → Effect (List AttackRoll.Result)
resolveAttacks { attackConfig, attackMods, attackCount, defense } = do
  attackResults <- List.fromFoldable <$> Array.fromFoldable <$> rollAttacks attackConfig attackMods attackCount
  case defense of
    Just { defenseConfig, defenseMods } -> pure attackResults
    Nothing -> pure attackResults
