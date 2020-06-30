port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LinkGenerator



---- MODEL ----


type alias Model =
    { playerState : PlayerState
    , text : String
    , roomID : String
    }


type alias PlayerMsgPayload =
    { message : PlayerMsg
    , data : String
    }


init : ( Model, Cmd Msg )
init =
    ( { playerState = Unstarted, text = "", roomID = "" }
    , Cmd.batch [ generateRoom, joinRoom "Sala2", sendPlayerMessage (LoadVideo "hBCUuSr-0Nk") ]
    )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "data", E.object [ ( "playerState", E.string (stringFromPlayerState model.playerState) ), ( "text", E.string model.text ) ] )
        , ( "roomID", E.string "Sala2" )
        ]


decoder : D.Decoder Model
decoder =
    D.map3 Model
        (D.at [ "data", "playerState" ] (D.map playerStateFromString D.string))
        (D.at [ "data", "text" ] D.string)
        (D.at [ "data", "roomID" ] D.string)


encodePlayerMsg : PlayerMsgPayload -> E.Value
encodePlayerMsg msgPayload =
    E.object
        [ ( "message", E.string (playMsgToString msgPayload.message) )
        , ( "data", E.string msgPayload.data )
        ]


playerStateFromString : String -> PlayerState
playerStateFromString stateString =
    case stateString of
        "Unstarted" ->
            Unstarted

        "Ended" ->
            Ended

        "Playing" ->
            Playing

        "Paused" ->
            Paused

        "Buffering" ->
            Buffering

        "VideoCued" ->
            VideoCued

        _ ->
            Unknown


stringFromPlayerState : PlayerState -> String
stringFromPlayerState playerState =
    case playerState of
        Unstarted ->
            "Unstarted"

        Ended ->
            "Ended"

        Playing ->
            "Playing"

        Paused ->
            "Paused"

        Buffering ->
            "Buffering"

        VideoCued ->
            "VideoCued"

        Unknown ->
            "Unknown"


playMsgToString : PlayerMsg -> String
playMsgToString msg =
    case msg of
        Play ->
            "play"

        Pause ->
            "pause"

        LoadVideo _ ->
            "loadVideo"


sendPlayerMessage : PlayerMsg -> Cmd msg
sendPlayerMessage playerMsg =
    let
        msg =
            case playerMsg of
                LoadVideo videoID ->
                    { message = LoadVideo videoID, data = videoID }

                _ ->
                    { message = playerMsg, data = "" }
    in
    emitPlayerMsg (encodePlayerMsg msg)


generateRoom : Cmd Msg
generateRoom =
    LinkGenerator.generate LinkGenerated



---- UPDATE ----


type Msg
    = SendData
    | ReceiveData E.Value
    | ReceivePlayerMsg String
    | TextChanged String
    | SetVideo
    | LinkGenerated String


type PlayerMsg
    = Play
    | Pause
    | LoadVideo String


type PlayerState
    = Unstarted
    | Ended
    | Playing
    | Paused
    | Buffering
    | VideoCued
    | Unknown


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData ->
            ( model, sendData (encode model) )

        SetVideo ->
            ( model
            , Cmd.batch
                [ sendData (encode model)
                , sendPlayerMessage (LoadVideo model.text)
                ]
            )

        TextChanged newText ->
            let
                newModel =
                    { model | text = newText }
            in
            ( newModel, Cmd.none )

        LinkGenerated link ->
            ( { model | roomID = link }, Cmd.none )

        ReceiveData value ->
            case D.decodeValue decoder value of
                Ok newModel ->
                    let
                        msgs =
                            if model.text == newModel.text then
                                []

                            else
                                [ sendPlayerMessage (LoadVideo newModel.text) ]
                    in
                    case newModel.playerState of
                        Playing ->
                            ( newModel, Cmd.batch (sendPlayerMessage Play :: msgs) )

                        Paused ->
                            ( newModel, Cmd.batch (sendPlayerMessage Pause :: msgs) )

                        _ ->
                            ( newModel, Cmd.batch msgs )

                Err e ->
                    ( model, Cmd.none )

        ReceivePlayerMsg playerMsg ->
            let
                newPlayerState =
                    playerStateFromString playerMsg

                newModel =
                    { model | playerState = newPlayerState }
            in
            ( newModel, sendData (encode newModel) )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [ id "player" ] []
        , h3 [] [ text model.text ]
        , input
            [ type_ "text"
            , placeholder "Video ID"
            , onInput TextChanged
            , value model.text
            ]
            []
        , button [ onClick SetVideo ] [ text "Set Video ID" ]
        ]



---- PORTS ----


port joinRoom : String -> Cmd msg


port sendData : E.Value -> Cmd msg


port emitPlayerMsg : E.Value -> Cmd msg


port dataReceiver : (E.Value -> msg) -> Sub msg


port playerMsgReceiver : (String -> msg) -> Sub msg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ dataReceiver ReceiveData, playerMsgReceiver ReceivePlayerMsg ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
