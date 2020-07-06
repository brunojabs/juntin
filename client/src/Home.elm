module Home exposing (Msg, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LinkGenerator


type Msg
    = LinkGenerated String
    | GenerateLink


view : Html Msg
view =
    div []
        [ img [ src "juntin-logo.png" ] []
        , h1 [] [ text "Bem vindo ao Juntin" ]
        , h2 [] [ text "Assista junto" ]
        , button [ onClick GenerateLink ] [ text "Crie uma sala" ]
        ]


generateRoom : Cmd Msg
generateRoom =
    LinkGenerator.generate LinkGenerated
