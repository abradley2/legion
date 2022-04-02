module Fields where

import Prelude
import AttackRoll as AttackRoll
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.List.NonEmpty as NEList
import Data.List.NonEmpty (NonEmptyList)
import Data.Tuple (Tuple(..))
import DefenseRoll as DefenseRoll
import Fields.Toggleable (Toggleable(..))

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

type ValidFields
  = { attackVariant :: AttackRoll.Variant
    , attackCount :: AttackCount
    , attackSurgeTokens :: Maybe AttackSurgeTokens
    , aimtokens :: Maybe AimTokens
    , defenseVariant :: Maybe DefenseRoll.Variant
    , defenseSurgeTokens :: Maybe DefenseSurgeTokens
    , dodgeTokens :: Maybe DodgeTokens
    , precise :: Maybe Precise
    , critical :: Maybe Critical
    , dangerSense :: Maybe DangerSense
    , cover :: Maybe Cover
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

type Validator from to
  = FieldInfo -> from -> Validation (NonEmptyList (Tuple FieldInfo String)) to

validateIsPositive :: forall field. Newtype field Int => Validator field field
validateIsPositive fieldInfo =
  unwrap
    >>> ( \v ->
          if v < 0 then
            Failure $ NEList.singleton (Tuple fieldInfo "Value must be greater than 0")
          else
            Success $ wrap v
      )

validateToggleable :: forall field. Newtype field Int => (Validator field field) -> (Validator (Toggleable field) (Maybe field))
validateToggleable validation fieldInfo = case _ of
  Disabled _ -> Success Nothing
  Enabled val -> Just <$> validation fieldInfo val

formValidation :: Fields -> Validation (NonEmptyList (Tuple FieldInfo String)) ValidFields
formValidation fields =
  ( pure
      { attackVariant: _
      , attackCount: _
      , attackSurgeTokens: _
      , aimtokens: _
      , defenseVariant: _
      , defenseSurgeTokens: _
      , dodgeTokens: _
      , precise: _
      , critical: _
      , dangerSense: _
      , cover: _
      , attackSurge: _
      , defenseSurge: _
      }
  )
    <*> pure fields.attackVariant
    <*> validateIsPositive attackCountField fields.attackCount
    <*> validateToggleable validateIsPositive attackSurgeTokensField fields.attackSurgeTokens
    <*> validateToggleable validateIsPositive aimTokensField fields.aimTokens
    <*> pure fields.defenseVariant
    <*> validateToggleable validateIsPositive defenseSurgeTokensField fields.defenseSurgeTokens
    <*> validateToggleable validateIsPositive dodgeTokensField fields.dodgeTokens
    <*> validateToggleable validateIsPositive preciseField fields.precise
    <*> validateToggleable validateIsPositive criticalField fields.critical
    <*> validateToggleable validateIsPositive dangerSenseField fields.dangerSense
    <*> validateToggleable validateIsPositive coverField fields.cover
    <*> pure fields.attackSurge
    <*> pure fields.defenseSurge

data Validation failure success
  = Failure failure
  | Success success

instance functorValidation :: Functor (Validation failure) where
  map _ (Failure failure) = Failure failure
  map fn (Success success) = Success (fn success)

instance applicativeValidation :: Semigroup failure => Applicative (Validation failure) where
  pure = Success

instance applyValidation :: Semigroup failure => Apply (Validation failure) where
  apply (Success _) (Failure failure) = Failure failure
  apply (Failure failure) (Success _) = Failure failure
  apply (Success fn) (Success val) = Success (fn val)
  apply (Failure failure1) (Failure failure2) = Failure (failure1 <> failure2)
