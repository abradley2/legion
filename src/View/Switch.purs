module View.Switch where

import Prelude
import Flame (Html)
import Flame.Html.Attribute as A
import Flame.Html.Event as E
import Flame.Html.Element as H

switch ::
  forall msg.
  Boolean ->
  (Boolean -> msg) ->
  { label :: String, id :: String } ->
  Html msg
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
