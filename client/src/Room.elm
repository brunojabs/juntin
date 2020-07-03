port module Room exposing (Model, Msg, RoomID, init, subscriptions, update, view)

import Html exposing (Html, button, div, h3, input, text)
import Html.Attributes exposing (id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E


type alias Model =
    { playerState : PlayerState
    , inputText : String
    , roomID : String
    }


type alias PlayerMsgPayload =
    { message : PlayerMsg
    , data : String
    }


type alias RoomID =
    String


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


encode : Model -> E.Value
encode model =
    E.object
        [ ( "data"
          , E.object
                [ ( "playerState", E.string (stringFromPlayerState model.playerState) )
                , ( "inputText", E.string model.inputText )
                ]
          )
        , ( "roomID", E.string model.roomID )
        ]


decoder : D.Decoder Model
decoder =
    D.map3 Model
        (D.at [ "data", "playerState" ] (D.map playerStateFromString D.string))
        (D.at [ "data", "text" ] D.string)
        (D.at [ "roomID" ] D.string)


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


init : RoomID -> ( Model, Cmd Msg )
init roomID =
    ( { playerState = Unstarted, inputText = "", roomID = roomID }
    , Cmd.batch [ joinRoom roomID, sendPlayerMessage (LoadVideo "hBCUuSr-0Nk") ]
    )


type Msg
    = SendData
    | ReceiveData E.Value
    | ReceivePlayerMsg String
    | TextChanged String
    | SetVideo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData ->
            ( model, sendData (encode model) )

        SetVideo ->
            ( model
            , Cmd.batch
                [ sendData (encode model)
                , sendPlayerMessage (LoadVideo model.inputText)
                ]
            )

        TextChanged newText ->
            let
                newModel =
                    { model | inputText = newText }
            in
            ( newModel, Cmd.none )

        ReceiveData value ->
            case D.decodeValue decoder value of
                Ok newModel ->
                    let
                        msgs =
                            if model.inputText == newModel.inputText then
                                []

                            else
                                [ sendPlayerMessage (LoadVideo newModel.inputText) ]
                    in
                    case newModel.playerState of
                        Playing ->
                            ( newModel, Cmd.batch (sendPlayerMessage Play :: msgs) )

                        Paused ->
                            ( newModel, Cmd.batch (sendPlayerMessage Pause :: msgs) )

                        _ ->
                            ( newModel, Cmd.batch msgs )

                Err e ->
                    let
                        _ =
                            Debug.log "error" e
                    in
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
        , h3 [] [ text model.inputText ]
        , input
            [ type_ "text"
            , placeholder "Video ID"
            , onInput TextChanged
            , value model.inputText
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ dataReceiver ReceiveData, playerMsgReceiver ReceivePlayerMsg ]
