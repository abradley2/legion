module View.RadioSelect where

import Prelude
import Flame (Html)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import Flame.Html.Event as E
import View.Icon (radioButton)

radioSelect ::
  forall msg.
  Boolean ->
  msg ->
  { label :: String
  , id :: String
  } ->
  Html msg
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
                , "flex-grow-0 flex-shrink-0 mr2 w1.5 h1.5": true
                }
            ]
            [ radioButton selected
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
