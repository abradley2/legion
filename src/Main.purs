module Main
  ( main
  ) where

import Prelude
import AttackRoll as AttackRoll
import CustomEvent (customEvent, dispatchDocumentEvent)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
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

type Model
  = { attackVariant :: AttackRoll.Variant
    , attackCount :: Int
    , attackSurgeTokens :: Toggleable Int
    , aimTokens :: Toggleable Int
    , defenseVariant :: Maybe DefenseRoll.Variant
    , defenseSurgeTokens :: Toggleable Int
    , dodgeTokens :: Toggleable Int
    }

data Msg
  = TriggerHelloEvent
  | ReceivedHelloEvent
  | AttackVariantSelected AttackRoll.Variant
  | DefenseVariantSelected (Maybe DefenseRoll.Variant)
  | AttackCountChanged String
  | DodgeTokensChanged (Toggleable Int)
  | DefenseSurgeTokensChanged (Toggleable Int)

triggerHelloEvent :: Aff (Maybe Msg)
triggerHelloEvent = liftEffect $ const Nothing <$> dispatchDocumentEvent (customEvent "hello" Object.empty)

init :: Tuple Model (Array (Aff (Maybe Msg)))
init =
  { attackVariant: AttackRoll.White
  , attackSurgeTokens: Disabled 0
  , defenseSurgeTokens: Disabled 0
  , dodgeTokens: Disabled 0
  , aimTokens: Disabled 0
  , defenseVariant: Just DefenseRoll.White
  , attackCount: 0
  }
    :> []

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
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

view :: Model -> Html Msg
view model =
  H.div
    [ A.class' "center measure pv3 avenir"
    ]
    [ H.div
        [ A.class' "flex flex-wrap na3"
        ]
        [ H.div
            [ A.class' "ma3"
            ]
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
                [ A.class' "mt3"
                ]
                [ H.label
                    [ A.class' "fw5" ]
                    [ H.text "Attack Count" ]
                , H.br
                , H.div
                    [ A.class' "inline-flex" ]
                    [ H.button
                        [ A.class' "self-stretch ba b--black-20 bg-black-70 w2 white pointer"
                        , E.onClick $ AttackCountChanged $ show $ model.attackCount - 1
                        ]
                        [ H.text "-"
                        ]
                    , H.input
                        [ A.type' "text"
                        , A.value $ show model.attackCount
                        , E.onInput AttackCountChanged
                        , A.class' "bt bb br-0 bl-0 pa2 w3 outline-0 b--black-20 tc"
                        ]
                    , H.button
                        [ A.class' "self-stretch ba b--black-20 bg-black-70 w2 white pointer"
                        , E.onClick $ AttackCountChanged $ show $ model.attackCount + 1
                        ]
                        [ H.text "+" ]
                    ]
                ]
            ]
        , H.div
            [ A.class' "ma3"
            ]
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
                    [ A.class' "mt3"
                    ]
                    [ H.div_
                        [ switch
                            (isToggled model.dodgeTokens)
                            (const $ DodgeTokensChanged $ toggle model.dodgeTokens)
                            { label: "Dodge Tokens", id: "dodge-tokens-switch" }
                        ]
                    ]
                Nothing -> H.text ""
            ]
        ]
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
