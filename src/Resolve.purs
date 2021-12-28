module Resolve where

import Prelude
import AttackRoll (rollAttacks)
import AttackRoll as AttackRoll
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (foldr)
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import DefenseRoll (rollBlocks)
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
    Just { defenseConfig, defenseMods } ->
      map (either (const (-1)) defenseResultToWounds)
        $ flip evalStateT defenseMods
        $ runExceptT (rollBlocks defenseConfig attackResults)
    Nothing ->
      pure $ attackResultToWounds attackResults

defenseResultToWounds :: List DefenseRoll.Result -> Int
defenseResultToWounds =
  foldr
    ( \result count -> case result of
        DefenseRoll.Wound -> count + 1
        _ -> count
    )
    0

attackResultToWounds :: List AttackRoll.Result -> Int
attackResultToWounds =
  foldr
    ( \result count -> case result of
        AttackRoll.Hit -> count + 1
        AttackRoll.Crit -> count + 1
        _ -> count
    )
    0
