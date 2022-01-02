module Fields where

import Prelude
import AttackRoll as AttackRoll
import Control.Monad.State (State, state)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import DefenseRoll as DefenseRoll

data Toggleable a
  = Enabled a
  | Disabled a

derive instance functorToggleable :: Functor Toggleable

isToggled :: forall a. Toggleable a -> Boolean
isToggled = case _ of
  Enabled _ -> true
  Disabled _ -> false

toggle :: forall a. Toggleable a -> Toggleable a
toggle = case _ of
  Enabled val -> Disabled val
  Disabled val -> Enabled val

unwrapToggleable :: forall a. Toggleable a -> a
unwrapToggleable = case _ of
  Enabled val -> val
  Disabled val -> val

getEnabled :: forall a. Toggleable a -> Maybe a
getEnabled = case _ of
  Enabled val -> Just val
  _ -> Nothing

type AttackVariant
  = AttackRoll.Variant

type DefenseVariant
  = DefenseRoll.Variant

newtype AttackCount
  = AttackCount Int

derive instance newtypeAttackCount :: Newtype AttackCount _

newtype AttackSurgeTokens
  = AttackSurgeTokens Int

derive instance newtypeAttackSurgeTokens :: Newtype AttackSurgeTokens _

newtype AimTokens
  = AimTokens Int

derive instance newtypeAimTokens :: Newtype AimTokens _

newtype DefenseSurgeTokens
  = DefenseSurgeTokens Int

derive instance newtypeDefenseSurgeTokens :: Newtype DefenseSurgeTokens _

newtype DodgeTokens
  = DodgeTokens Int

derive instance newtypeDodgeTokens :: Newtype DodgeTokens _

newtype Precise
  = Precise Int

derive instance newtypePrecise :: Newtype Precise _

newtype Critical
  = Critical Int

derive instance newtypeCritical :: Newtype Critical _

newtype DangerSense
  = DangerSense Int

derive instance newtypeDangerSense :: Newtype DangerSense _

newtype Cover
  = Cover Int

derive instance newtypeCover :: Newtype Cover _

data DefenseSurge
  = SurgeBlock

derive instance eqDefenseSurge :: Eq DefenseSurge

data AttackSurge
  = SurgeHit
  | SurgeCrit

derive instance eqAttackSurge :: Eq AttackSurge

type Fields
  = { attackVariant :: AttackRoll.Variant
    , attackCount :: AttackCount
    , attackSurgeTokens :: Toggleable AttackSurgeTokens
    , aimTokens :: Toggleable AimTokens
    , defenseVariant :: Maybe DefenseRoll.Variant
    , defenseSurgeTokens :: Toggleable DefenseSurgeTokens
    , dodgeTokens :: Toggleable DodgeTokens
    , precise :: Toggleable Precise
    , critical :: Toggleable Critical
    , dangerSense :: Toggleable DangerSense
    , cover :: Toggleable Cover
    , attackSurge :: Maybe AttackSurge
    , defenseSurge :: Maybe DefenseSurge
    }

init :: Fields
init =
  { attackVariant: AttackRoll.White
  , attackSurgeTokens: Disabled (AttackSurgeTokens 0)
  , defenseSurgeTokens: Disabled (DefenseSurgeTokens 0)
  , dodgeTokens: Disabled (DodgeTokens 0)
  , aimTokens: Disabled (AimTokens 0)
  , precise: Disabled (Precise 0)
  , critical: Disabled (Critical 0)
  , dangerSense: Disabled (DangerSense 0)
  , cover: Disabled (Cover 0)
  , defenseVariant: Just DefenseRoll.White
  , attackCount: AttackCount 0
  , attackSurge: Nothing
  , defenseSurge: Nothing
  }

type ValidFields
  = { attackVariant :: AttackVariant
    , attackCount :: AttackCount
    , defenseVariant :: Maybe DefenseVariant
    , attackSurgeTokens :: Maybe AttackSurgeTokens
    , defenseSurgeTokens :: Maybe DefenseSurgeTokens
    , aimTokens :: Maybe AimTokens
    , dodgeTokens :: Maybe DodgeTokens
    , precise :: Maybe Precise
    , critical :: Maybe Critical
    , dangerSense :: Maybe DangerSense
    , cover :: Maybe Cover
    , attackSurge :: Maybe AttackSurge
    , defenseSurge :: Maybe DefenseSurge
    }

isPositive :: String -> Int -> State (List String) (Maybe Int)
isPositive name val =
  if val < 0 then
    state (\errors -> Tuple Nothing (Cons (name <> " cannot be less than 0") errors))
  else
    pure $ Just val

isEnabledPositive :: String -> Toggleable Int -> State (List String) (Maybe Int)
isEnabledPositive name =
  getEnabled
    >>> case _ of
        Just val -> isPositive name val
        Nothing -> pure Nothing

validateForm :: Fields -> State (List String) (Maybe ValidFields)
validateForm fields = do
  attackCount <- do
    val <- isPositive "Attack Count" (unwrap fields.attackCount)
    pure $ AttackCount <$> val
  defenseVariant <- pure $ Just fields.defenseVariant
  attackSurgeTokens <- do
    val <- isEnabledPositive "Attack Surge Tokens" (unwrap <$> fields.attackSurgeTokens)
    pure $ Just $ AttackSurgeTokens <$> val
  defenseSurgeTokens <- do
    val <- isEnabledPositive "Defense Surge Tokens" (unwrap <$> fields.defenseSurgeTokens)
    pure $ Just $ DefenseSurgeTokens <$> val
  aimTokens <- do
    val <- isEnabledPositive "Aim Tokens" (unwrap <$> fields.aimTokens)
    pure $ Just $ AimTokens <$> val
  dodgeTokens <- do
    val <- isEnabledPositive "Dodge Tokens" (unwrap <$> fields.aimTokens)
    pure $ Just $ DodgeTokens <$> val
  precise <- do
    val <- isEnabledPositive "Precise" (unwrap <$> fields.precise)
    pure $ Just $ Precise <$> val
  critical <- do
    val <- isEnabledPositive "Critical" (unwrap <$> fields.critical)
    pure $ Just $ Critical <$> val
  dangerSense <- do
    val <- isEnabledPositive "Danger Sense" (unwrap <$> fields.dangerSense)
    pure $ Just $ DangerSense <$> val
  cover <- do
    val <- isEnabledPositive "Cover" (unwrap <$> fields.cover)
    pure $ Just $ Cover <$> val
  pure
    $ { attackVariant: _
      , attackSurge: _
      , defenseSurge: _
      , attackCount: _
      , defenseVariant: _
      , attackSurgeTokens: _
      , defenseSurgeTokens: _
      , aimTokens: _
      , dodgeTokens: _
      , precise: _
      , critical: _
      , dangerSense: _
      , cover: _
      }
    <$> Just fields.attackVariant
    <*> Just fields.attackSurge
    <*> Just fields.defenseSurge
    <*> attackCount
    <*> defenseVariant
    <*> attackSurgeTokens
    <*> defenseSurgeTokens
    <*> aimTokens
    <*> dodgeTokens
    <*> precise
    <*> critical
    <*> dangerSense
    <*> cover
