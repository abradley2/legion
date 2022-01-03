module DropdownMenu where

import Prelude
import Flame (Html)
import Flame.Html.Element as H
import Flame.Html.Event as E
import Flame.Html.Attribute as A

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
        [ A.class' "h2.5 ba b--black-80 bg-transparent w4 outline-0 pa2 f5 lh-title pointer"
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
