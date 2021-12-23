module Resolve where

import Prelude
import AttackRoll (rollAttacks)
import AttackRoll as AttackRoll
import Control.Monad.State (State, get, state)
import Data.Array (catMaybes)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DefenseRoll (rollDefense)
import DefenseRoll as DefenseRoll
import Effect (Effect)

resolveAttacks ::
  AttackRoll.Variant ->
  ( AttackRoll.Result ->
    Maybe AttackRoll.Value
  ) ->
  (Maybe DefenseRoll.Variant) ->
  Int ->
  Effect { attackResults :: Array (AttackRoll.Value), defenseResults :: Array (DefenseRoll.Value) }
resolveAttacks attackVar attackSurge defenseVar attackCount = do
  attackResults <- List.fromFoldable <<< catMaybes <$> rollAttacks attackVar attackSurge attackCount
  pure { attackResults: [], defenseResults: [] }

type DefenseMods
  = { dodge :: Int
    , cover :: Int
    }

resolveDefenseMods :: List AttackRoll.Value -> State DefenseMods (List AttackRoll.Value)
resolveDefenseMods Nil = do pure Nil

resolveDefenseMods (Cons attack next) = do
  result <- applyMod attack
  case result of
    Just hit -> (Cons hit) <$> resolveDefenseMods next
    Nothing -> resolveDefenseMods next
  where
    applyMod :: AttackRoll.Value -> State DefenseMods (Maybe AttackRoll.Value)
    applyMod AttackRoll.Crit = pure $ Just AttackRoll.Crit

    applyMod AttackRoll.Hit =
      state
        ( \{ dodge, cover } ->
            if dodge > 0 then
              Tuple Nothing { dodge: dodge - 1, cover }
            else if cover > 0 then
              Tuple Nothing { dodge, cover: cover - 1 }
            else
              Tuple (Just AttackRoll.Hit) { dodge, cover }
        )
