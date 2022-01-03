module ToggleGroup where

import Prelude
import Data.Maybe (Maybe(..))
import Fields (Toggleable(..), isToggled, toggle, unwrapToggleable)
import Flame (Html)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import NumberInput (numberInput)
import Switch (switch)

toggleGroup ::
  forall msg.
  { label :: String, id :: String, onChange :: (Toggleable Int -> msg), value :: Toggleable Int } ->
  Array (Html msg)
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
      , A.style1 "transition" "height 0.33s"
      ]
      [ numberInput
          { label: Nothing
          , value: unwrapToggleable value
          , id: id <> "-input"
          , onChange: Enabled >>> onChange
          }
      ]
  ]
