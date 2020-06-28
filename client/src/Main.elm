port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E



---- MODEL ----


type alias Model =
    { paused : Bool
    , text : String
    }


init : ( Model, Cmd Msg )
init =
    ( { paused = True, text = "" }, joinRoom "Sala2" )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "data", E.object [ ( "paused", E.bool model.paused ), ( "text", E.string model.text ) ] )
        , ( "roomID", E.string "Sala2" )
        ]


decoder : D.Decoder Model
decoder =
    D.map2 Model
        (D.at [ "data", "paused" ] D.bool)
        (D.at [ "data", "text" ] D.string)



---- UPDATE ----


type Msg
    = Join
    | SendData
    | ReceiveData E.Value
    | Pause
    | TextChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData ->
            ( model, sendData (encode model) )

        Join ->
            ( model, joinRoom "Sala2" )

        Pause ->
            let
                paused =
                    if model.paused then
                        False

                    else
                        True

                newModel =
                    { model | paused = paused }
            in
            ( newModel, sendData (encode newModel) )

        TextChanged newText ->
            let
                newModel =
                    { model | text = newText }
            in
            ( newModel, sendData (encode newModel) )

        ReceiveData value ->
            case D.decodeValue decoder value of
                Ok newModel ->
                    ( newModel, Cmd.none )

                Err e ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , h2 []
            [ text
                ("Is Paused: "
                    ++ (if model.paused then
                            "Yes"

                        else
                            "No"
                       )
                )
            ]
        , h3 [] [ text model.text ]
        , button [ onClick Pause ] [ text "Pause" ]
        , input
            [ type_ "text"
            , placeholder "Draft"
            , onInput TextChanged
            , value model.text
            ]
            []
        ]



---- PORTS ----


port joinRoom : String -> Cmd msg


port sendData : E.Value -> Cmd msg


port dataReceiver : (E.Value -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    dataReceiver ReceiveData



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
