module Home exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import LinkGenerator
import Logo exposing (logo)


type Msg
    = LinkGenerated String
    | GenerateLink


type Model
    = Model Nav.Key


init : Nav.Key -> ( Model, Cmd Msg )
init key =
    ( Model key, Cmd.none )


view : Html Msg
view =
    div [ class "home" ]
        [ logo
        , h1 [] [ text "Bem-vindo ao Juntin" ]
        , h2 [] [ text "Assista junto" ]
        , button [ onClick GenerateLink, class "button" ] [ text "Crie uma sala" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model key) =
    case msg of
        LinkGenerated link ->
            ( Model key, Nav.pushUrl key link )

        GenerateLink ->
            ( Model key, generateRoom )


generateRoom : Cmd Msg
generateRoom =
    LinkGenerator.generate LinkGenerated
