module Main (main) where

import Prelude
import AttackRoll as AttackRoll
import Data.Array ((..), length)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence, sum)
import Data.Tuple (Tuple)
import DefenseRoll as DefenseRoll
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Fields (AimTokens, AttackCount(..), AttackSurge, AttackSurgeTokens, Cover(..), Critical, DangerSense(..), DefenseSurge, DefenseSurgeTokens, DodgeTokens(..), FieldInfo, Fields, Precise, validateForm)
import Fields as Fields
import Fields.Toggleable (Toggleable)
import Flame (Html, QuerySelector(..), mount_, (:>))
import Flame.Application.EffectList (Application)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import Flame.Html.Event as E
import Resolve (resolveAttacks)
import View.DropdownMenu (dropdownMenu)
import View.NumberInput (numberInput)
import View.RadioSelect (radioSelect)
import View.ToggleGroup (toggleGroup)

average :: Array Int -> Number
average arr = toNumber (sum arr) / toNumber (length arr)

type Model
  = { fields :: Fields
    , dropdownOpen :: Boolean
    , formErrors :: Maybe (List FieldInfo)
    , result :: Maybe (Array Int)
    }

data Msg
  = SubmitFormClicked
  | AttacksResolved (Array Int)
  | AttackVariantSelected AttackRoll.Variant
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
  { fields: Fields.init
  , dropdownOpen: false
  , formErrors: Nothing
  , result: Nothing
  }
    :> []

updateFields :: Model -> (Fields -> Fields) -> Model
updateFields model fn = model { fields = fn model.fields }

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
update model (AttacksResolved wounds) =
  model { formErrors = Nothing, result = Just wounds }
    :> []

update model SubmitFormClicked = case validateForm model.fields of
  Left formErrors -> model { formErrors = Just formErrors } :> []
  Right config ->
    model
      :> [ map (AttacksResolved >>> Just)
            $ liftEffect
            $ sequence
            $ (const $ resolveAttacks config)
            <$> (1 .. 1000)
        ]

update model (ToggleDropdown dropdownOpen) =
  model { dropdownOpen = dropdownOpen }
    :> []

update model (AttackSurgeChanged attackSurge) =
  updateFields model (_ { attackSurge = attackSurge })
    :> []

update model (DefenseSurgeChanged defenseSurge) =
  updateFields model (_ { defenseSurge = defenseSurge })
    :> []

update model (DangerSenseChanged dangerSense) =
  updateFields model (_ { dangerSense = dangerSense })
    :> []

update model (CoverChanged cover) =
  updateFields model (_ { cover = cover })
    :> []

update model (PreciseChanged precise) =
  updateFields model (_ { precise = precise })
    :> []

update model (CriticalChanged critical) =
  updateFields model (_ { critical = critical })
    :> []

update model (AttackSurgeTokensChanged attackSurgeTokens) =
  updateFields model (_ { attackSurgeTokens = attackSurgeTokens })
    :> []

update model (AimTokensChanged aimTokens) =
  updateFields model (_ { aimTokens = aimTokens })
    :> []

update model (DefenseSurgeTokensChanged defenseSurgeTokens) =
  updateFields model (_ { defenseSurgeTokens = defenseSurgeTokens })
    :> []

update model (DodgeTokensChanged dodgeTokens) =
  updateFields model (_ { dodgeTokens = dodgeTokens })
    :> []

update model (AttackCountChanged attackCount) =
  updateFields model (_ { attackCount = attackCount })
    :> []

update model (DefenseVariantSelected defenseVariant) =
  updateFields model (_ { defenseVariant = defenseVariant })
    :> []

update model (AttackVariantSelected attackVariant) =
  updateFields model (_ { attackVariant = attackVariant })
    :> []

view :: Model -> Html Msg
view model =
  H.div
    [ A.class' "flex flex-wrap justify-center ma3 avenir"
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
    , H.div
        [ A.class' "w-100 tc pa3" ]
        [ H.div
            [ A.class' "inline-flex flex-column"
            ]
            [ H.button
                [ E.onClick SubmitFormClicked ]
                [ H.text "Try Submit"
                ]
            , H.div
                [ A.class' "mt3"
                ]
                [ case { formErrors: model.formErrors, result: model.result } of
                    { formErrors: Just _, result: _ } -> H.text "There are errors"
                    { result: Just result } -> H.text $ show (average result) <> " wounds dealt"
                    _ -> H.text ""
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
                (value == model.fields.attackVariant)
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
                (value == model.fields.attackSurge)
                (AttackSurgeChanged value)
                { label, id }
          )
            <$> [ { value: Nothing, label: "( No Surge )", id: "attack-surge-none" }
              , { value: Just AttackRoll.Hit, label: "Surge Hit", id: "attack-surge-hit" }
              , { value: Just AttackRoll.Crit, label: "Surge Crit", id: "attack-surge-crit" }
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
                , value: unwrap model.fields.attackCount
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

attackModGridView :: Model -> Html Msg
attackModGridView model =
  H.div
    [ A.class' "flex flex-wrap na3" ]
    [ H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "attack-surge-tokens"
            , label: "Surge Tokens"
            , value: unwrap <$> model.fields.attackSurgeTokens
            , onChange: map wrap >>> AttackSurgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "aim-tokens"
            , label: "Aim Tokens"
            , value: unwrap <$> model.fields.aimTokens
            , onChange: map wrap >>> AimTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "critical"
            , label: "Critical X"
            , value: unwrap <$> model.fields.critical
            , onChange: map wrap >>> CriticalChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "precise"
            , label: "Precise X"
            , value: unwrap <$> model.fields.precise
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
                (value == model.fields.defenseVariant)
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
                (value == model.fields.defenseSurge)
                (DefenseSurgeChanged value)
                { label, id }
          )
            <$> [ { value: Nothing, label: "( No Surge )", id: "defense-surge-none" }
              , { value: Just DefenseRoll.Block, label: "Surge Block", id: "defense-surge-block" }
              ]
        )
    , case model.fields.defenseVariant of
        Just _ ->
          H.div
            [ A.class' "mt3" ]
            [ defenseModGridView model ]
        Nothing -> H.text ""
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
            , value: unwrap <$> model.fields.defenseSurgeTokens
            , onChange: map wrap >>> DefenseSurgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "dodge-tokens"
            , label: "Dodge Tokens"
            , value: unwrap <$> model.fields.dodgeTokens
            , onChange: map DodgeTokens >>> DodgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "danger-sense"
            , label: "Danger Sense"
            , value: unwrap <$> model.fields.dangerSense
            , onChange: map DangerSense >>> DangerSenseChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "cover"
            , label: "Cover X"
            , value: unwrap <$> model.fields.cover
            , onChange: map Cover >>> CoverChanged
            }
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
