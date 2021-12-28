module Icon where

import Flame (Html)
import Flame.Html.Element as H
import Flame.Html.Attribute as A

materialSvg ∷ forall msg. Array (Html msg) -> Html msg
materialSvg =
  H.svg
    [ A.fill "currentColor"
    , A.viewBox "0 0 24 24"
    ]

radioButton :: forall msg. Boolean -> Html msg
radioButton = case _ of
  true ->
    materialSvg
      [ H.path'
          [ A.d "M12 7c-2.76 0-5 2.24-5 5s2.24 5 5 5 5-2.24 5-5-2.24-5-5-5zm0-5C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"
          ]
      ]
  false ->
    materialSvg
      [ H.path'
          [ A.d "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10 10-4.48 10-10S17.52 2 12 2zm0 18c-4.42 0-8-3.58-8-8s3.58-8 8-8 8 3.58 8 8-3.58 8-8 8z"
          ]
      ]
