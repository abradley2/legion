module Main
  ( main
  ) where

import Prelude
import CustomEvent (customEvent, dispatchDocumentEvent)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested (tuple3)
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
  = { count :: Int
    }

data Msg
  = Count
  | TriggerHelloEvent
  | ReceivedHelloEvent

triggerHelloEvent :: Aff (Maybe Msg)
triggerHelloEvent = liftEffect $ const Nothing <$> dispatchDocumentEvent (customEvent "hello" Object.empty)

init :: Tuple Model (Array (Aff (Maybe Msg)))
init =
  { count: 0 }
    :> []

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
update model Count =
  model { count = model.count + 1 }
    :> []

update model TriggerHelloEvent = model :> [ triggerHelloEvent ]

update model ReceivedHelloEvent =
  model { count = model.count + 1000 }
    :> []

view :: Model -> Html Msg
view model =
  H.div_
    [ H.button
        [ E.onClick Count
        ]
        [ H.text $ "Clicked " <> show model.count <> " times" ]
    , H.button
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
