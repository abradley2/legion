module View.NumberInput where

import Prelude
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Flame (Html)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import Flame.Html.Event as E
import View.Icon as Icon

numberInput :: forall msg. { label :: Maybe String, id :: String, value :: Int, onChange :: Int -> msg } -> Html msg
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
            [ A.class' "self-stretch ba b--black-20 bg-black-70 white pointer"
            , E.onClick $ onChange $ value - 1
            ]
            [ H.span
                [ A.class' "dib pt1 w1.5 h1.5"
                ]
                [ Icon.removeCircle
                ]
            ]
        , H.input
            [ A.type' "text"
            , A.id id
            , A.value $ show value
            , E.onInput $ Int.fromString >>> fromMaybe 0 >>> onChange
            , A.class' "bt bb br-0 bl-0 pa2 w3 outline-0 b--black-20 tc f5 lh-title"
            ]
        , H.button
            [ A.class' "self-stretch ba b--black-20 bg-black-70 white pointer"
            , E.onClick $ onChange $ value + 1
            ]
            [ H.span
                [ A.class' "dib pt1"
                , A.style1 "width" "1.5rem"
                , A.style1 "height" "1.5rem"
                ]
                [ Icon.addCircle
                ]
            ]
        ]
    ]
