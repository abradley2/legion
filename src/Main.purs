module Main
  ( Model
  , Msg
  , Toggleable
  , attackModGridView
  , attackConfigView
  , defenseConfigView
  , main
  , toggleGroup
  , update
  , view
  ) where

import Prelude
import AttackRoll as AttackRoll
import CustomEvent (customEvent, dispatchDocumentEvent)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (tuple3)
import DefenseRoll as DefenseRoll
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Flame (Html, QuerySelector(..), mount_, (:>))
import Flame.Application.EffectList (Application)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import Flame.Html.Event as E
import Flame.Types (Source(..))
import Foreign.Object as Object
import Icon as Icon

data Toggleable a
  = Enabled a
  | Disabled a

isToggled :: forall a. Toggleable a -> Boolean
isToggled = case _ of
  Enabled _ -> true
  Disabled _ -> false

toggle :: forall a. Toggleable a -> Toggleable a
toggle = case _ of
  Enabled val -> Disabled val
  Disabled val -> Enabled val

toggleableVal :: forall a. Toggleable a -> a
toggleableVal = case _ of
  Enabled val -> val
  Disabled val -> val

type Model
  = { attackVariant :: AttackRoll.Variant
    , attackCount :: Int
    , attackSurgeTokens :: Toggleable Int
    , aimTokens :: Toggleable Int
    , defenseVariant :: Maybe DefenseRoll.Variant
    , defenseSurgeTokens :: Toggleable Int
    , dodgeTokens :: Toggleable Int
    , precise :: Toggleable Int
    , critical :: Toggleable Int
    }

data Msg
  = TriggerHelloEvent
  | ReceivedHelloEvent
  | AttackVariantSelected AttackRoll.Variant
  | DefenseVariantSelected (Maybe DefenseRoll.Variant)
  | AttackCountChanged String
  | AimTokensChanged (Toggleable Int)
  | AttackSurgeTokensChanged (Toggleable Int)
  | DodgeTokensChanged (Toggleable Int)
  | DefenseSurgeTokensChanged (Toggleable Int)
  | PreciseChanged (Toggleable Int)
  | CriticalChanged (Toggleable Int)

triggerHelloEvent :: Aff (Maybe Msg)
triggerHelloEvent = liftEffect $ const Nothing <$> dispatchDocumentEvent (customEvent "hello" Object.empty)

init :: Tuple Model (Array (Aff (Maybe Msg)))
init =
  { attackVariant: AttackRoll.White
  , attackSurgeTokens: Disabled 0
  , defenseSurgeTokens: Disabled 0
  , dodgeTokens: Disabled 0
  , aimTokens: Disabled 0
  , precise: Disabled 0
  , critical: Disabled 0
  , defenseVariant: Just DefenseRoll.White
  , attackCount: 0
  }
    :> []

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
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

update model (AttackCountChanged count) =
  model { attackCount = fromMaybe 0 $ Int.fromString count }
    :> []

update model (DefenseVariantSelected defenseVariant) =
  model { defenseVariant = defenseVariant }
    :> []

update model (AttackVariantSelected attackVariant) =
  model { attackVariant = attackVariant }
    :> []

update model TriggerHelloEvent = model :> [ triggerHelloEvent ]

update model ReceivedHelloEvent = model :> []

numberInput :: forall msg. { label :: Maybe String, id :: String, value :: Int, onChange :: String -> msg } -> Html msg
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
            [ A.class' "self-stretch ba b--black-20 bg-black-70 w2 white pointer"
            , E.onClick $ onChange $ show $ value - 1
            ]
            [ H.text "-"
            ]
        , H.input
            [ A.type' "text"
            , A.id id
            , A.value $ show value
            , E.onInput onChange
            , A.class' "bt bb br-0 bl-0 pa2 w3 outline-0 b--black-20 tc"
            ]
        , H.button
            [ A.class' "self-stretch ba b--black-20 bg-black-70 w2 white pointer"
            , E.onClick $ onChange $ show $ value + 1
            ]
            [ H.text "+" ]
        ]
    ]

view :: Model -> Html Msg
view model =
  H.div
    [ A.class' "flex justify-center ma3 avenir"
    ]
    [ H.div
        [ A.class' "flex flex-wrap na3 items-start mw7"
        ]
        [ H.div
            [ A.class' "pa3 w-50" ]
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
            [ A.class' "pa3 w-50"
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
        [ A.class' "flex flex-column"
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
        [ A.class' "mv3" ]
        [ numberInput
            { id: "attack-count-input"
            , label: Just "Attack Count"
            , value: model.attackCount
            , onChange: AttackCountChanged
            }
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
          , value: toggleableVal value
          , id: id <> "-input"
          , onChange: Int.fromString >>> fromMaybe 0 >>> Enabled >>> onChange
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
            , value: model.attackSurgeTokens
            , onChange: AttackSurgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "aim-tokens"
            , label: "Aim Tokens"
            , value: model.aimTokens
            , onChange: AimTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "critical"
            , label: "Critical N"
            , value: model.critical
            , onChange: CriticalChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "precise"
            , label: "Precise N"
            , value: model.precise
            , onChange: PreciseChanged
            }
    ]

defenseConfigView :: Model -> Html Msg
defenseConfigView model =
  H.div_
    [ H.div
        [ A.class' "flex flex-column" ]
        ( ( \{ value, label, id } ->
              radioSelect
                (value == model.defenseVariant)
                (DefenseVariantSelected value)
                { label, id }
          )
            <$> [ { value: Nothing, label: "(No Defense)", id: "no-defense-variant" }
              , { value: Just DefenseRoll.White, label: "White", id: "white-defense-variant" }
              , { value: Just DefenseRoll.Red, label: "Red", id: "red-defense-variant" }
              ]
        )
    , case model.defenseVariant of
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
            , value: model.defenseSurgeTokens
            , onChange: DefenseSurgeTokensChanged
            }
    , H.div
        [ A.class' "ma3" ]
        $ toggleGroup
            { id: "dodge-tokens"
            , label: "Dodge Tokens"
            , value: model.dodgeTokens
            , onChange: DodgeTokensChanged
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
  , subscribe:
      [ tuple3 Document "hello" (const ReceivedHelloEvent)
      ]
  }

main :: Effect Unit
main = mount_ (QuerySelector "#app") app
