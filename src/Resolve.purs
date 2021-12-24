module Resolve where

import Prelude
import AttackRoll (rollAttacks)
import AttackRoll as AttackRoll
import Control.Monad.State (State, evalState, state)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DefenseRoll (rollDefense)
import DefenseRoll as DefenseRoll
import Effect (Effect)

type Config
  = { attackVariant :: AttackRoll.Variant
    , attackSurge :: AttackRoll.Result -> Maybe AttackRoll.Value
    , attackCount :: Int
    , defense :: Maybe DefenseConfig
    }

type DefenseConfig
  = { defenseMods :: DefenseMods
    , defenseVariant :: DefenseRoll.Variant
    , defenseSurge :: DefenseRoll.Result -> Maybe DefenseRoll.Value
    }

resolveAttacks ∷ Config → Effect (List AttackRoll.Value)
resolveAttacks { attackVariant, attackSurge, attackCount, defense } = do
  attackResults <- List.fromFoldable <$> catMaybes <$> Array.fromFoldable <$> rollAttacks attackVariant attackSurge attackCount
  case defense of
    Just defenseConfig ->
      resolveDefenseRolls defenseConfig
        $ evalState (resolveDefenseMods attackResults) defenseConfig.defenseMods
    Nothing -> pure attackResults

type DefenseMods
  = { dodge :: Int
    , cover :: Int
    }

resolveDefenseRolls :: DefenseConfig -> List AttackRoll.Value -> Effect (List AttackRoll.Value)
resolveDefenseRolls config = case _ of
  Nil -> pure Nil
  (Cons attack attacks) -> do
    result <- rollDefense config.defenseVariant config.defenseSurge
    case result of
      Just DefenseRoll.Block -> resolveDefenseRolls config attacks
      Nothing -> Cons attack <$> resolveDefenseRolls config attacks

resolveDefenseMods :: List AttackRoll.Value -> State DefenseMods (List AttackRoll.Value)
resolveDefenseMods = case _ of
  Nil -> pure Nil
  Cons AttackRoll.Crit attacks -> Cons AttackRoll.Crit <$> resolveDefenseMods attacks
  Cons AttackRoll.Hit attacks -> do
    result <-
      state
        $ \{ dodge, cover } ->
            if cover > 0 then
              Tuple Nothing { dodge, cover: cover - 1 }
            else if dodge > 0 then
              Tuple Nothing { dodge: dodge - 1, cover }
            else
              Tuple (Just AttackRoll.Hit) { dodge, cover }
    case result of
      Just hit -> Cons hit <$> resolveDefenseMods attacks
      Nothing -> resolveDefenseMods attacks
