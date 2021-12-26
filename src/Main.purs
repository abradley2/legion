module Main
  ( main
  )
  where

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
import Flame.Html.Element as H
import Flame.Html.Event as E
import Flame.Types (Source(..))
import Foreign.Object as Object

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
  H.div_
    [ H.button
        [ E.onClick TriggerHelloEvent
        ]
        [ H.text "Trigger external event" ]
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
