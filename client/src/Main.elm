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


sendPlayerMessage : PlayerMsg -> Cmd msg
sendPlayerMessage playerMsg =
    case playerMsg of
        Play ->
            emitPlayerMsg "play"

        Pause ->
            emitPlayerMsg "pause"



---- UPDATE ----


type Msg
    = Join
    | SendData
    | ReceiveData E.Value
    | TextChanged String
    | PlayVideo
    | PauseVideo


type PlayerMsg
    = Play
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData ->
            ( model, sendData (encode model) )

        Join ->
            ( model, joinRoom "Sala2" )

        PauseVideo ->
            let
                newModel =
                    { model | paused = True }
            in
            ( newModel
            , Cmd.batch
                [ sendData (encode newModel)
                , sendPlayerMessage Pause
                ]
            )

        TextChanged newText ->
            let
                newModel =
                    { model | text = newText }
            in
            ( newModel, sendData (encode newModel) )

        ReceiveData value ->
            case D.decodeValue decoder value of
                Ok newModel ->
                    if newModel.paused then
                        ( newModel, sendPlayerMessage Pause )

                    else
                        ( newModel, sendPlayerMessage Play )

                Err e ->
                    ( model, Cmd.none )

        PlayVideo ->
            let
                newModel =
                    { model | paused = False }
            in
            ( newModel
            , Cmd.batch
                [ sendData (encode newModel)
                , sendPlayerMessage Play
                ]
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ id "player" ] []
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
        , button [ onClick PauseVideo ] [ text "Pause" ]
        , button [ onClick PlayVideo ] [ text "Play" ]
        ]



---- PORTS ----


port joinRoom : String -> Cmd msg


port sendData : E.Value -> Cmd msg


port emitPlayerMsg : String -> Cmd msg


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
