module Fields where

import Prelude
import AttackRoll as AttackRoll
import Control.Monad.State (State, runState, state)
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..))
import DefenseRoll as DefenseRoll
import Fields.Toggleable (Toggleable(..), getEnabled)
import Resolve (Config)

type FieldInfo
  = { errorLabel :: String
    , id :: String
    }

type AttackVariant
  = AttackRoll.Variant

attackVariantField :: FieldInfo
attackVariantField = { id: "attack-variant", errorLabel: "Attack Variant" }

type DefenseVariant
  = DefenseRoll.Variant

defenseVariantField :: FieldInfo
defenseVariantField = { id: "defense-variant", errorLabel: "Defense Variant" }

newtype AttackCount
  = AttackCount Int

derive instance newtypeAttackCount :: Newtype AttackCount _

attackCountField :: FieldInfo
attackCountField = { id: "attack-count", errorLabel: "Attack Count" }

newtype AttackSurgeTokens
  = AttackSurgeTokens Int

derive instance newtypeAttackSurgeTokens :: Newtype AttackSurgeTokens _

attackSurgeTokensField :: FieldInfo
attackSurgeTokensField = { id: "attack-surge-tokens", errorLabel: "Attack Surge Tokens" }

newtype AimTokens
  = AimTokens Int

derive instance newtypeAimTokens :: Newtype AimTokens _

aimTokensField :: FieldInfo
aimTokensField = { id: "aim-tokens", errorLabel: "Aim Tokens" }

newtype DefenseSurgeTokens
  = DefenseSurgeTokens Int

derive instance newtypeDefenseSurgeTokens :: Newtype DefenseSurgeTokens _

defenseSurgeTokensField :: FieldInfo
defenseSurgeTokensField = { id: "defense-surge-tokens", errorLabel: "Defense Surge Tokens" }

newtype DodgeTokens
  = DodgeTokens Int

derive instance newtypeDodgeTokens :: Newtype DodgeTokens _

dodgeTokensField :: FieldInfo
dodgeTokensField = { id: "dodge-tokens", errorLabel: "Dodge Tokens" }

newtype Precise
  = Precise Int

derive instance newtypePrecise :: Newtype Precise _

preciseField :: FieldInfo
preciseField = { id: "precise", errorLabel: "Precise" }

newtype Critical
  = Critical Int

derive instance newtypeCritical :: Newtype Critical _

criticalField :: FieldInfo
criticalField = { id: "critical", errorLabel: "Critical" }

newtype DangerSense
  = DangerSense Int

derive instance newtypeDangerSense :: Newtype DangerSense _

dangerSenseField :: FieldInfo
dangerSenseField = { id: "danger-sense", errorLabel: "Danger Sense" }

newtype Cover
  = Cover Int

derive instance newtypeCover :: Newtype Cover _

coverField :: FieldInfo
coverField = { id: "cover", errorLabel: "Cover" }

type DefenseSurge
  = DefenseRoll.Result

defenseSurgeField :: FieldInfo
defenseSurgeField = { id: "defense-surge", errorLabel: "Defense Surge" }

type AttackSurge
  = AttackRoll.Result

attackSurgeField :: FieldInfo
attackSurgeField = { id: "attack-surge", errorLabel: "Attack Surge" }

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

isPositive :: FieldInfo -> Int -> State (List FieldInfo) (Maybe Int)
isPositive fieldInfo val =
  if val < 0 then
    state (\errors -> Tuple Nothing (Cons (fieldInfo { errorLabel = fieldInfo.errorLabel <> " cannot be less than 0" }) errors))
  else
    pure $ Just val

isEnabledPositive :: FieldInfo -> Toggleable Int -> State (List FieldInfo) (Maybe Int)
isEnabledPositive fieldInfo =
  getEnabled
    >>> case _ of
        Just val -> isPositive fieldInfo val
        Nothing -> pure Nothing

validateForm :: Fields -> Either (List FieldInfo) Config
validateForm =
  validateForm_
    >>> flip runState Nil
    >>> case _ of
        Tuple _ (Cons err errors) -> Left (Cons err errors)
        Tuple (Just validFields) Nil -> Right validFields
        Tuple Nothing Nil -> Left (Cons { id: "unknown", errorLabel: "Failed to validate form: unknown error" } Nil)

validateForm_ :: Fields -> State (List FieldInfo) (Maybe Config)
validateForm_ fields = do
  attackCount <-
    map AttackCount
      <$> isPositive attackCountField (unwrap fields.attackCount)
  attackSurgeTokens <-
    map AttackSurgeTokens
      <$> isEnabledPositive attackSurgeTokensField (unwrap <$> fields.attackSurgeTokens)
  defenseSurgeTokens <-
    map DefenseSurgeTokens
      <$> isEnabledPositive defenseSurgeTokensField (unwrap <$> fields.defenseSurgeTokens)
  aimTokens <-
    map AimTokens
      <$> isEnabledPositive aimTokensField (unwrap <$> fields.aimTokens)
  dodgeTokens <-
    map DodgeTokens
      <$> isEnabledPositive dodgeTokensField (unwrap <$> fields.aimTokens)
  precise <-
    map Precise
      <$> isEnabledPositive preciseField (unwrap <$> fields.precise)
  critical <-
    map Critical
      <$> isEnabledPositive criticalField (unwrap <$> fields.critical)
  dangerSense <-
    map DangerSense
      <$> isEnabledPositive dangerSenseField (unwrap <$> fields.dangerSense)
  cover <-
    map Cover
      <$> isEnabledPositive coverField (unwrap <$> fields.cover)
  pure
    $ ( \(AttackCount attackCount') ->
          { attackCount: attackCount'
          , attackConfig:
              { surge: fields.attackSurge
              , variant: fields.attackVariant
              }
          , attackMods:
              { rerolls: maybe 0 (\v -> v * 2) $ unwrap <$> aimTokens
              , surgeTokens: fromMaybe 0 $ unwrap <$> attackSurgeTokens
              }
          , defense: Nothing
          }
      )
    <$> attackCount
