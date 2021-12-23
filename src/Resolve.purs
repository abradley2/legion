module Resolve where

import Prelude
import AttackRoll (rollAttacks)
import AttackRoll as AttackRoll
import Control.Monad.State (State, get, state, evalState)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DefenseRoll (rollDefense)
import DefenseRoll as DefenseRoll
import Effect (Effect)

resolveAttacks attackVar attackSurge attackCount = do
  attackResults <- List.fromFoldable <$> catMaybes <$> rollAttacks attackVar attackSurge attackCount
  let finalAttackResults = Array.fromFoldable $ evalState (resolveDefenseMods attackResults) { dodge: 0, cover: 0 }
  pure finalAttackResults

type DefenseMods
  = { dodge :: Int
    , cover :: Int
    }

resolveDefenseMods :: List AttackRoll.Value -> State DefenseMods (List AttackRoll.Value)
resolveDefenseMods = case _ of
  Nil -> pure Nil
  (Cons attack next) -> do
    result <- applyMod attack
    case result of
      Just hit -> (Cons hit) <$> resolveDefenseMods next
      Nothing -> resolveDefenseMods next
  where
  applyMod :: AttackRoll.Value -> State DefenseMods (Maybe AttackRoll.Value)
  applyMod = case _ of
    AttackRoll.Crit -> pure $ Just AttackRoll.Crit
    AttackRoll.Hit ->
      state
        ( \{ dodge, cover } ->
            if dodge > 0 then
              Tuple Nothing { dodge: dodge - 1, cover }
            else if cover > 0 then
              Tuple Nothing { dodge, cover: cover - 1 }
            else
              Tuple (Just AttackRoll.Hit) { dodge, cover }
        )
