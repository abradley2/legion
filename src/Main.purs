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

type Model
  = { attackVariant :: AttackRoll.Variant
    , attackCount :: Int
    , attackSurgeTokens :: Int
    , aimTokens :: Int
    , defenseVariant :: Maybe DefenseRoll.Variant
    , defenseCount :: Int
    , defenseSurgeTokens :: Int
    , dodgeTokens :: Int
    }

data Msg
  = TriggerHelloEvent
  | ReceivedHelloEvent
  | AttackVariantSelected AttackRoll.Variant
  | DefenseVariantSelected (Maybe DefenseRoll.Variant)
  | AttackCountChanged String
  | DefenseCountChanged String

triggerHelloEvent :: Aff (Maybe Msg)
triggerHelloEvent = liftEffect $ const Nothing <$> dispatchDocumentEvent (customEvent "hello" Object.empty)

init :: Tuple Model (Array (Aff (Maybe Msg)))
init =
  { attackVariant: AttackRoll.White
  , attackSurgeTokens: 0
  , defenseSurgeTokens: 0
  , dodgeTokens: 0
  , aimTokens: 0
  , defenseVariant: Nothing
  , attackCount: 0
  , defenseCount: 0
  }
    :> []

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
update model (AttackCountChanged count) =
  model { attackCount = fromMaybe 0 $ Int.fromString count }
    :> []

update model (DefenseCountChanged count) =
  model { defenseCount = fromMaybe 0 $ Int.fromString count }
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
        [ A.class' "flex"
        ]
        [ H.div_
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
                [ H.label_ [ H.text "Attack Count" ]
                , H.br
                , H.input
                    [ A.type' "number"
                    , A.value $ show model.attackCount
                    , E.onInput AttackCountChanged
                    ]
                ]
            ]
        , H.div_
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
                    [ H.text "Defense Selected"
                    ]
                Nothing -> H.text ""
            ]
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
    [ A.class' ""
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
                , "black-80": selected
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
                { "black-20": not selected
                , "black-80 fw6": selected
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
