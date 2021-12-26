module Resolve where

import Prelude
import AttackRoll (rollAttacks)
import AttackRoll as AttackRoll
import Data.Foldable (foldr)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import DefenseRoll as DefenseRoll
import Effect (Effect)

type Config
  = { attackConfig :: AttackRoll.Config
    , attackMods :: AttackRoll.Mods
    , attackCount :: Int
    , defense :: Maybe { defenseConfig :: DefenseRoll.Config, defenseMods :: DefenseRoll.Mods }
    }

resolveAttacks ∷ Config → Effect Int
resolveAttacks { attackConfig, attackMods, attackCount, defense } = do
  attackResults <- List.fromFoldable <$> Array.fromFoldable <$> rollAttacks attackConfig attackMods attackCount
  case defense of
    Just { defenseConfig, defenseMods } -> do
      pure 0
    Nothing ->
      pure
        $ foldr
            ( \res count -> case res of
                AttackRoll.Hit -> count + 1
                AttackRoll.Crit -> count + 1
                _ -> count
            )
            0
            attackResults
