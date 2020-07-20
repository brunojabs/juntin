module ControlButton exposing (pauseButtonView, playButtonView)

import Html exposing (div)
import Html.Attributes as HTMLAtt
import Svg exposing (g, svg)
import Svg.Attributes exposing (..)


pauseButtonView =
    div [ HTMLAtt.class "control-button__wrapper" ]
        [ svg [ viewBox "0 0 19.106 19.461" ] [ g [ transform "translate(-70.58 -135.26)" ] [ Svg.path [ d "m71.479 136.16v17.662h5.9852v-17.662h-5.9852zm11.324 0v17.662h5.9852v-17.662h-5.9852z", stroke "#000", strokeLinejoin "round", strokeWidth "1.7971" ] [] ] ]
        ]


playButtonView =
    div [ HTMLAtt.class "control-button__wrapper" ]
        [ svg
            [ viewBox "0 0 19.106 19.461"
            ]
            [ g
                [ transform "translate(-70.58 -135.26)"
                ]
                [ Svg.path
                    [ d "m71.637 136.26-0.0569 17.461 17.106-8.5926z"
                    , stroke "#000"
                    , strokeLinejoin "round"
                    , strokeWidth "2"
                    ]
                    []
                ]
            ]
        ]
