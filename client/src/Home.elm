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
        [ h1 [] [ text "Bem vindo ao Juntin" ]
        , h2 [] [ text "Plataforma para assistir à videos no Youtube em companhia" ]
        , button [ onClick GenerateLink ] [ text "Set Video ID" ]
        ]


generateRoom : Cmd Msg
generateRoom =
    LinkGenerator.generate LinkGenerated
