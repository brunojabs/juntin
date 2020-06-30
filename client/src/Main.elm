port module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LinkGenerator
import Url exposing (Url)
import Url.Parser as UrlParser



---- MODEL ----


type alias Model =
    { playerState : PlayerState
    , text : String
    , roomID : String
    , currentRoute : Route
    }


type alias PlayerMsgPayload =
    { message : PlayerMsg
    , data : String
    }


type Route
    = Root
    | Room String
    | NotFound


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Root UrlParser.top
        , UrlParser.map Room UrlParser.string
        ]


parseRoute : Url -> Route
parseRoute url =
    UrlParser.parse route url |> Maybe.withDefault NotFound


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    let
        initialRoute =
            parseRoute url

        roomID =
            case initialRoute of
                Room a ->
                    a

                _ ->
                    ""

        joinCmd =
            case initialRoute of
                Room b ->
                    joinRoom b

                _ ->
                    Cmd.none
    in
    ( { playerState = Unstarted, text = "", roomID = roomID, currentRoute = initialRoute }
    , Cmd.batch [ joinCmd, sendPlayerMessage (LoadVideo "hBCUuSr-0Nk") ]
    )


encode : Model -> E.Value
encode model =
    E.object
        [ ( "data", E.object [ ( "playerState", E.string (stringFromPlayerState model.playerState) ), ( "text", E.string model.text ) ] )
        , ( "roomID", E.string model.roomID )
        ]


decoder : D.Decoder Model
decoder =
    D.map4 Model
        (D.at [ "data", "playerState" ] (D.map playerStateFromString D.string))
        (D.at [ "data", "text" ] D.string)
        (D.at [ "roomID" ] D.string)
        (D.succeed NotFound)


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
    | UrlChanged
    | UrlRequested


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

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Juntin"
    , body = [ mainView model ]
    }


mainView : Model -> Html Msg
mainView model =
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
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> UrlRequested
        , onUrlChange = \_ -> UrlChanged
        }
