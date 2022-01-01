module Main where

import Prelude
import AttackRoll as AttackRoll
import Control.Monad.State (State, state)
import Data.Function (applyFlipped)
import Data.Int as Int
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (Tuple(..))
import DefenseRoll as DefenseRoll
import Effect (Effect)
import Effect.Aff (Aff)
import Flame (Html, QuerySelector(..), mount_, (:>))
import Flame.Application.EffectList (Application)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import Flame.Html.Event as E
import Icon as Icon

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

data DefenseSurge
  = SurgeBlock

derive instance eqDefenseSurge :: Eq DefenseSurge

data AttackSurge
  = SurgeHit
  | SurgeCrit

derive instance eqAttackSurge :: Eq AttackSurge

type Model
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
    , dropdownOpen :: Boolean
    }

newtype AttackCount
  = AttackCount Int

derive instance newtypeAttackCount :: Newtype AttackCount _

newtype AttackSurgeTokens
  = AttackSurgeTokens Int

derive instance newtypeAttackSurgeTokens :: Newtype AttackSurgeTokens _

newtype AimTokens
  = AimTokens Int

derive instance newtypeAimTokens :: Newtype AimTokens _

newtype DefenseVariant
  = DefenseVariant DefenseRoll.Variant

derive instance newtypeDefenseVariant :: Newtype DefenseVariant _

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

type ValidForm
  = { attackVariant :: AttackRoll.Variant
    , attackCount :: AttackCount
    , defenseVariant :: Maybe DefenseRoll.Variant
    , attackSurgeTokens :: Maybe AttackSurgeTokens
    , defenseSurgeTokens :: Maybe DefenseSurgeTokens
    , aimTokens :: Maybe AimTokens
    , dodgeTokens :: Maybe DodgeTokens
    , precise :: Maybe Precise
    , critical :: Maybe Critical
    , dangerSense :: Maybe DangerSense
    , cover :: Maybe Cover
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

validateModel' :: Model -> State (List String) (Maybe ValidForm)
validateModel' m = do
  attackCount <- do
    val <- isPositive "Attack Count" (unwrap m.attackCount)
    pure $ AttackCount <$> val
  defenseVariant <- pure $ Just m.defenseVariant
  attackSurgeTokens <- do
    val <- isEnabledPositive "Attack Surge Tokens" (unwrap <$> m.attackSurgeTokens)
    pure $ Just $ AttackSurgeTokens <$> val
  defenseSurgeTokens <- do
    val <- isEnabledPositive "Defense Surge Tokens" (unwrap <$> m.defenseSurgeTokens)
    pure $ Just $ DefenseSurgeTokens <$> val
  aimTokens <- do
    val <- isEnabledPositive "Aim Tokens" (unwrap <$> m.aimTokens)
    pure $ Just $ AimTokens <$> val
  dodgeTokens <- do
    val <- isEnabledPositive "Dodge Tokens" (unwrap <$> m.aimTokens)
    pure $ Just $ DodgeTokens <$> val
  precise <- do
    val <- isEnabledPositive "Precise" (unwrap <$> m.precise)
    pure $ Just $ Precise <$> val
  critical <- do
    val <- isEnabledPositive "Critical" (unwrap <$> m.critical)
    pure $ Just $ Critical <$> val
  dangerSense <- do
    val <- isEnabledPositive "Danger Sense" (unwrap <$> m.dangerSense)
    pure $ Just $ DangerSense <$> val
  cover <- do
    val <- isEnabledPositive "Cover" (unwrap <$> m.cover)
    pure $ Just $ Cover <$> val
  pure
    $ { attackVariant: _
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
    <$> Just m.attackVariant
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

data Msg
  = AttackVariantSelected AttackRoll.Variant
  | DefenseVariantSelected (Maybe DefenseRoll.Variant)
  | AttackCountChanged AttackCount
  | AimTokensChanged (Toggleable AimTokens)
  | AttackSurgeTokensChanged (Toggleable AttackSurgeTokens)
  | DodgeTokensChanged (Toggleable DodgeTokens)
  | DefenseSurgeTokensChanged (Toggleable DefenseSurgeTokens)
  | PreciseChanged (Toggleable Precise)
  | CriticalChanged (Toggleable Critical)
  | DangerSenseChanged (Toggleable DangerSense)
  | CoverChanged (Toggleable Cover)
  | AttackSurgeChanged (Maybe AttackSurge)
  | DefenseSurgeChanged (Maybe DefenseSurge)
  | ToggleDropdown Boolean

init :: Tuple Model (Array (Aff (Maybe Msg)))
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
  , dropdownOpen: false
  }
    :> []

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
update model (ToggleDropdown dropdownOpen) =
  model { dropdownOpen = dropdownOpen }
    :> []

update model (AttackSurgeChanged attackSurge) =
  model { attackSurge = attackSurge }
    :> []

update model (DefenseSurgeChanged defenseSurge) =
  model { defenseSurge = defenseSurge }
    :> []

update model (DangerSenseChanged dangerSense) =
  model { dangerSense = dangerSense }
    :> []

update model (CoverChanged cover) =
  model { cover = cover }
    :> []

update model (PreciseChanged precise) =
  model { precise = precise }
    :> []

update model (CriticalChanged critical) =
  model { critical = critical }
    :> []

update model (AttackSurgeTokensChanged attackSurgeTokens) =
  model { attackSurgeTokens = attackSurgeTokens }
    :> []

update model (AimTokensChanged aimTokens) =
  model { aimTokens = aimTokens }
    :> []

update model (DefenseSurgeTokensChanged defenseSurgeTokens) =
  model { defenseSurgeTokens = defenseSurgeTokens }
    :> []

update model (DodgeTokensChanged dodgeTokens) =
  model { dodgeTokens = dodgeTokens }
    :> []

update model (AttackCountChanged attackCount) =
  model { attackCount = attackCount }
    :> []

update model (DefenseVariantSelected defenseVariant) =
  model { defenseVariant = defenseVariant }
    :> []

update model (AttackVariantSelected attackVariant) =
  model { attackVariant = attackVariant }
    :> []

numberInput :: forall msg. { label :: Maybe String, id :: String, value :: Int, onChange :: Int -> msg } -> Html msg
numberInput { label, id, value, onChange } =
  H.div
    [ A.class' "dib"
    ]
    [ case label of
        Just labelText ->
          H.label
            [ A.class' "fw5"
            , A.for id
            ]
            [ H.text labelText ]
        _ -> H.text ""
    , maybe (H.text "") (const H.br) label
    , H.div
        [ A.class' "inline-flex" ]
        [ H.button
            [ A.class' "self-stretch ba b--black-20 bg-black-70 white pointer"
            , E.onClick $ onChange $ value - 1
            ]
            [ H.span
                [ A.class' "dib pt1"
                , A.style1 "width" "1.5rem"
                , A.style1 "height" "1.5rem"
                ]
                [ Icon.removeCircle
                ]
            ]
        , H.input
            [ A.type' "text"
            , A.id id
            , A.value $ show value
            , E.onInput $ Int.fromString >>> fromMaybe 0 >>> onChange
            , A.class' "bt bb br-0 bl-0 pa2 w3 outline-0 b--black-20 tc f5 lh-title"
            ]
        , H.button
            [ A.class' "self-stretch ba b--black-20 bg-black-70 white pointer"
            , E.onClick $ onChange $ value + 1
            ]
            [ H.span
                [ A.class' "dib pt1"
                , A.style1 "width" "1.5rem"
                , A.style1 "height" "1.5rem"
                ]
                [ Icon.addCircle
                ]
            ]
        ]
    ]

view :: Model -> Html Msg
view model =
  H.div
    [ A.class' "flex justify-center ma3 avenir"
    ]
    [ H.div
        [ A.class' "flex flex-column items-center items-start-l flex-row-l flex-wrap-l na3 items-start"
        , A.style1 "max-width" "50rem"
        ]
        [ H.div
            [ A.class' "pa3 w-50-l" ]
            [ H.div
                [ A.class' "pa4 shadow-5" ]
                [ H.div
                    [ A.class' "tc black-80 f3 pb1 pt2 mb2 bb b--black-80 nl4 nr4 nt4 tracked ttu" ]
                    [ H.text "Attack"
                    ]
                , attackConfigView model
                ]
            ]
        , H.div
            [ A.class' "pa3 w-50-l"
            ]
            [ H.div
                [ A.class' "pa4 shadow-5" ]
                [ H.div
                    [ A.class' "tc black-80 f3 pb1 pt2 mb2 bb b--black-80 nl4 nr4 nt4 tracked ttu"
                    ]
                    [ H.text "Defense"
                    ]
                , defenseConfigView model
                ]
            ]
        ]
    ]

attackConfigView :: Model -> Html Msg
attackConfigView model =
  H.div_
    [ H.div
        [ A.class' "inline-flex flex-column w-50"
        ]
        ( ( \{ value, label, id } ->
              radioSelect
                (value == model.attackVariant)
                (AttackVariantSelected value)
                { label, id }
          )
            <$> [ { value: AttackRoll.White, label: "White", id: "white-attack-variant" }
              , { value: AttackRoll.Black, label: "Black", id: "black-attack-variant" }
              , { value: AttackRoll.Red, label: "Red", id: "red-attack-variant" }
              ]
        )
    , H.div
        [ A.class' "inline-flex flex-column w-50" ]
        ( ( \{ value, label, id } ->
              radioSelect
                (value == model.attackSurge)
                (AttackSurgeChanged value)
                { label, id }
          )
            <$> [ { value: Nothing, label: "( No Surge )", id: "attack-surge-none" }
              , { value: Just SurgeHit, label: "Surge Hit", id: "attack-surge-hit" }
              , { value: Just SurgeCrit, label: "Surge Crit", id: "attack-surge-crit" }
              ]
        )
    , H.div
        [ A.class' "mv3 nl3 nr3 flex"
        ]
        [ H.div
            [ A.class' "mh3" ]
            [ numberInput
                { id: "attack-count-input"
                , label: Just "Attack Count"
                , value: unwrap model.attackCount
                , onChange: AttackCount >>> AttackCountChanged
                }
            ]
        , H.div
            [ A.class' "mh3" ]
            [ dropdownMenu
                { isOpen: model.dropdownOpen
                , toggleOpen: ToggleDropdown
                , id: "sample-dropdown"
                }
            ]
        ]
    , attackModGridView model
    ]

toggleGroup :: { label :: String, id :: String, onChange :: (Toggleable Int -> Msg), value :: Toggleable Int } -> Array (Html Msg)
toggleGroup { label, id, onChange, value } =
  [ switch
      (isToggled value)
      (const $ onChange $ toggle value)
      { label, id: id <> "-switch" }
  , H.div
      [ A.class'
          { "overflow-hidden": true
          , "h3 pv2": isToggled value
          , "h0": not $ isToggled value
          }
      , A.style1 "transition" "0.33s"
      ]
      [ numberInput
          { label: Nothing
          , value: unwrapToggleable value
          , id: id <> "-input"
          , onChange: Enabled >>> onChange
          }
      ]
  ]

attackModGridView :: Model -> Html Msg
attackModGridView model =
  H.div
    [ A.class' "flex flex-wrap na3" ]
    [ H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "attack-surge-tokens"
            , label: "Surge Tokens"
            , value: unwrap <$> model.attackSurgeTokens
            , onChange: map wrap >>> AttackSurgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "aim-tokens"
            , label: "Aim Tokens"
            , value: unwrap <$> model.aimTokens
            , onChange: map wrap >>> AimTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "critical"
            , label: "Critical X"
            , value: unwrap <$> model.critical
            , onChange: map wrap >>> CriticalChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "precise"
            , label: "Precise X"
            , value: unwrap <$> model.precise
            , onChange: map wrap >>> PreciseChanged
            }
    ]

defenseConfigView :: Model -> Html Msg
defenseConfigView model =
  H.div_
    [ H.div
        [ A.class' "inline-flex w-50 flex-column" ]
        ( ( \{ value, label, id } ->
              radioSelect
                (value == model.defenseVariant)
                (DefenseVariantSelected value)
                { label, id }
          )
            <$> [ { value: Nothing, label: "( No Defense )", id: "no-defense-variant" }
              , { value: Just DefenseRoll.White, label: "White", id: "white-defense-variant" }
              , { value: Just DefenseRoll.Red, label: "Red", id: "red-defense-variant" }
              ]
        )
    , H.div
        [ A.class' "inline-flex w-50 flex-column pl2" ]
        ( ( \{ value, label, id } ->
              radioSelect
                (value == model.defenseSurge)
                (DefenseSurgeChanged value)
                { label, id }
          )
            <$> [ { value: Nothing, label: "( No Surge )", id: "defense-surge-none" }
              , { value: Just SurgeBlock, label: "Surge Block", id: "defense-surge-block" }
              ]
        )
    , case model.defenseVariant of
        Just _ ->
          H.div
            [ A.class' "mt3" ]
            [ defenseModGridView model ]
        Nothing -> H.text ""
    ]

dropdownMenu :: forall msg. { isOpen :: Boolean, toggleOpen :: Boolean -> msg, id :: String } -> Html msg
dropdownMenu { isOpen, toggleOpen, id } =
  H.div
    [ A.class' "inline-flex flex-column"
    ]
    [ H.label
        [ A.class' "fw5"
        ]
        [ H.text "label" ]
    , H.button
        [ A.class' "ba b--black-80 bg-transparent w4 outline-0 pa2 f5 lh-title pointer"
        , A.style1 "height" "2.5rem"
        , E.onClick $ toggleOpen (not isOpen)
        , A.createAttribute "aria-controls" id
        , A.createAttribute "aria-expanded" $ show isOpen
        ]
        [ H.text "_"
        ]
    , H.div
        [ A.class' "relative w-100 overflow-visible"
        ]
        [ H.createElementNode "focus-menu"
            [ A.class'
                { "absolute left-0 right--1 z-1 flex flex-column items-stretch": true
                , "bg-white pa2 ba b--black o-100 shadow-5 overflow-y-scroll": isOpen
                , "o-0 overflow-hidden": not isOpen
                }
            , A.style1 "max-height" if isOpen then "15rem" else "0rem"
            , A.style1 "top" if isOpen then "0.25rem" else "3rem"
            , A.style1 "transition" "0.33s"
            , A.createAttribute "show" $ show isOpen
            , E.createEvent "requestedclose" (toggleOpen false)
            , A.id id
            ]
            [ H.button
                [ A.class' "hover-bg-blue" ]
                [ H.text "Option one" ]
            , H.button_
                [ H.text "Option two" ]
            , H.button_
                [ H.text "Option three" ]
            ]
        ]
    ]

defenseModGridView :: Model -> Html Msg
defenseModGridView model =
  H.div
    [ A.class' "flex flex-wrap na3" ]
    [ H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "defense-surge-tokens"
            , label: "Surge Tokens"
            , value: unwrap <$> model.defenseSurgeTokens
            , onChange: map wrap >>> DefenseSurgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "dodge-tokens"
            , label: "Dodge Tokens"
            , value: unwrap <$> model.dodgeTokens
            , onChange: map DodgeTokens >>> DodgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "danger-sense"
            , label: "Danger Sense"
            , value: unwrap <$> model.dangerSense
            , onChange: map DangerSense >>> DangerSenseChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "cover"
            , label: "Cover X"
            , value: unwrap <$> model.cover
            , onChange: map Cover >>> CoverChanged
            }
    ]

switch ::
  Boolean ->
  (Boolean -> Msg) ->
  { label :: String, id :: String } ->
  Html Msg
switch isOn onChange { label, id } =
  H.div
    [ A.class' "dib"
    ]
    [ H.input
        [ A.type' "checkbox"
        , A.class' "dn"
        , A.id id
        , A.name id
        , A.checked isOn
        , E.onCheck onChange
        ]
    , H.label
        [ A.class' "dib pointer"
        , A.for id
        ]
        [ H.div
            [ A.class' "inline-flex" ]
            [ H.div
                [ A.class'
                    { "dib center br-pill ba b--black-50 relative": true
                    , "bg-white": not isOn
                    , "bg-light-gray": isOn
                    }
                , A.style
                    { "width": "2rem"
                    , "height": ".75rem"
                    }
                ]
                [ H.div'
                    [ A.class'
                        { "dib br-100 absolute ba b--black": true
                        , "bg-gray": isOn
                        , "bg-light-gray": not isOn
                        }
                    , A.style
                        { "width": "1.2rem"
                        , "height": "1.2rem"
                        , "top": "calc(50% - 0.6rem)"
                        , "transition": "0.33s"
                        , "left":
                            if not isOn then
                              "calc(0% - 0.2rem)"
                            else
                              "calc(100% - 1rem)"
                        }
                    ]
                ]
            , H.div
                [ A.class'
                    { "dib f7 pl2": true
                    , "black-50": not isOn
                    , "black-80": isOn
                    }
                ]
                [ H.text $ if isOn then "( Enabled )" else "( Disabled )" ]
            ]
        , H.br
        , H.span
            [ A.class'
                { "black-50": not isOn
                , "black-70 fw6": isOn
                , "f5": true
                }
            ]
            [ H.text label ]
        ]
    ]

radioSelect ::
  Boolean ->
  Msg ->
  { label :: String
  , id :: String
  } ->
  Html Msg
radioSelect selected onSelect { label, id } =
  H.div
    [ A.class' "dib"
    ]
    [ H.input
        [ A.type' "radio"
        , A.class' "dn"
        , A.id id
        , A.name id
        , A.checked selected
        , E.onCheck $ const onSelect
        ]
    , H.label
        [ A.class' "inline-flex items-center pointer"
        , A.for id
        ]
        [ H.div
            [ A.class'
                { "black-20": not selected
                , "black-70": selected
                , "flex-grow-0 flex-shrink-0 mr2": true
                }
            , A.style
                { "height": "1.5rem"
                , "width": "1.5rem"
                }
            ]
            [ Icon.radioButton selected
            ]
        , H.div
            [ A.class'
                { "black-50": not selected
                , "black-70 fw6": selected
                , "mt1 f5": true
                }
            ]
            [ H.text label
            ]
        ]
    ]

app :: Application Model Msg
app =
  { init
  , update
  , view
  , subscribe: []
  }

main :: Effect Unit
main = mount_ (QuerySelector "#app") app
