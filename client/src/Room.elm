port module Room exposing (Model, Msg, RoomID, init, subscriptions, update, view)

import Html exposing (Html, button, div, h3, img, input, label, text)
import Html.Attributes exposing (class, for, hidden, id, name, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Player exposing (..)


type alias RoomID =
    String


type Model
    = Joining RoomID
    | WaitingSync RoomID
    | Loaded LoadedModel


type alias LoadedModel =
    { inputText : String
    , room : Room
    , roomID : RoomID
    }


type Msg
    = SetVideo
    | TextChanged String
    | JoinedRoom Int
    | ReceiveData E.Value
    | ReceivePlayerMsg String
    | ReceiveCurrentTimeForSync Float


type BroadcastMsg
    = AskForData
    | SendData Room
    | Play
    | Pause
    | SetCurrentVideo String


type alias Room =
    { currentVideoID : String
    , currentTime : Float
    , playerState : Player.State
    }


initialModel : RoomID -> Model
initialModel roomID =
    Loaded
        { inputText = ""
        , room = initialRoom
        , roomID = roomID
        }


initialRoom =
    { currentVideoID = "hBCUuSr-0Nk"
    , currentTime = 0
    , playerState = Player.Unstarted
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Joining roomID, JoinedRoom roomSize ) ->
            if roomSize == 1 then
                ( initialModel roomID
                , sendPlayerMessage (Player.CueVideo initialRoom.currentVideoID initialRoom.currentTime)
                )

            else
                ( WaitingSync roomID, sendData (encode roomID AskForData) )

        ( WaitingSync roomID, ReceiveData data ) ->
            case D.decodeValue decoder data of
                Ok (SendData room) ->
                    let
                        playCommand =
                            case room.playerState of
                                Player.Playing ->
                                    Player.LoadVideo

                                _ ->
                                    Player.CueVideo
                    in
                    ( updateModelRoom room (initialModel roomID)
                    , sendPlayerMessage (playCommand room.currentVideoID room.currentTime)
                    )

                Ok _ ->
                    ( model, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        ( Loaded { room, roomID }, ReceiveData data ) ->
            case D.decodeValue decoder data of
                Ok AskForData ->
                    ( model, sendPlayerMessage GetCurrentTime )

                Ok Play ->
                    ( updateModelRoom room model, sendPlayerMessage Player.Play )

                Ok Pause ->
                    ( updateModelRoom { room | playerState = Paused } model, sendPlayerMessage Player.Pause )

                Ok (SetCurrentVideo videoID) ->
                    ( updateModelRoom { room | currentVideoID = videoID } model
                    , sendPlayerMessage (Player.LoadVideo videoID 0)
                    )

                Ok _ ->
                    ( model, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        ( Loaded _, TextChanged text ) ->
            ( updateModelInputText text model, Cmd.none )

        ( Loaded { inputText, room, roomID }, SetVideo ) ->
            let
                videoID =
                    Player.youtubeIdFromUrl inputText |> Maybe.withDefault ""

                newModel =
                    model
                        |> updateModelInputText ""
                        |> updateModelRoom { room | currentVideoID = videoID }

                cmds =
                    Cmd.batch
                        [ sendPlayerMessage (Player.LoadVideo videoID 0)
                        , sendData (encode roomID (SetCurrentVideo videoID))
                        ]
            in
            ( newModel, cmds )

        ( Loaded { room, roomID }, ReceivePlayerMsg playerMsg ) ->
            let
                newPlayerState =
                    playerStateFromString playerMsg

                newModel =
                    updateModelRoom { room | playerState = newPlayerState } model

                cmds =
                    case newPlayerState of
                        Playing ->
                            sendData (encode roomID Play)

                        Paused ->
                            sendData (encode roomID Pause)

                        _ ->
                            Cmd.none
            in
            ( newModel, cmds )

        ( Loaded { roomID, room }, ReceiveCurrentTimeForSync newCurrentTime ) ->
            let
                newRoom =
                    { room | currentTime = newCurrentTime }
            in
            ( updateModelRoom newRoom model, sendData (encode roomID (SendData newRoom)) )

        ( _, _ ) ->
            ( model, Cmd.none )


init : RoomID -> ( Model, Cmd Msg )
init roomID =
    ( Joining roomID
    , joinRoom roomID
    )


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                Joining roomID ->
                    text ("Entrando na sala: " ++ roomID)

                WaitingSync roomID ->
                    text ("Aguardando infos da sala: " ++ roomID)

                Loaded room ->
                    div
                        [ class "video-url" ]
                        [ div [ class "video-url__group" ]
                            [ input
                                [ type_ "text"
                                , placeholder "Cole aqui o link do vídeo"
                                , onInput TextChanged
                                , value room.inputText
                                , class "video-url__input"
                                , name "video-url-input"
                                , id "video-url-input"
                                ]
                                []
                            , label
                                [ class "video-url__label", for "video-url-input" ]
                                [ text "Cole aqui o link do vídeo" ]
                            ]
                        , button [ onClick SetVideo, class "video-url__button button" ] [ text "enviar" ]
                        ]

        hidePlayer =
            case model of
                Loaded _ ->
                    False

                _ ->
                    True
    in
    div [ class "room" ]
        [ img [ src "juntin-logo.png" ] []
        , div [ id "player", hidden hidePlayer ] []
        , content
        ]


encode : RoomID -> BroadcastMsg -> E.Value
encode roomID broadcastMsg =
    case broadcastMsg of
        SendData room ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "playerState", E.string (stringFromPlayerState room.playerState) )
                        , ( "currentVideoID", E.string room.currentVideoID )
                        , ( "currentTime", E.float room.currentTime )
                        ]
                  )
                , ( "roomID", E.string roomID )
                , ( "message", E.string "SendData" )
                ]

        AskForData ->
            E.object
                [ ( "roomID", E.string roomID )
                , ( "message", E.string "AskForData" )
                ]

        Play ->
            E.object
                [ ( "roomID", E.string roomID )
                , ( "message", E.string "Play" )
                ]

        Pause ->
            E.object
                [ ( "roomID", E.string roomID )
                , ( "message", E.string "Pause" )
                ]

        SetCurrentVideo videoID ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "videoID", E.string videoID )
                        ]
                  )
                , ( "roomID", E.string roomID )
                , ( "message", E.string "SetCurrentVideo" )
                ]


decoder : D.Decoder BroadcastMsg
decoder =
    D.field "message" D.string |> D.andThen messageDecoder


messageDecoder : String -> D.Decoder BroadcastMsg
messageDecoder message =
    case message of
        "AskForData" ->
            D.succeed AskForData

        "Play" ->
            D.succeed Play

        "Pause" ->
            D.succeed Pause

        "SetCurrentVideo" ->
            D.at [ "data", "videoID" ] D.string |> D.map SetCurrentVideo

        "SendData" ->
            roomDecoder |> D.map SendData

        _ ->
            D.fail ":("


roomDecoder : D.Decoder Room
roomDecoder =
    D.map3 Room
        (D.at [ "data", "currentVideoID" ] D.string)
        (D.at [ "data", "currentTime" ] D.float)
        (D.at [ "data", "playerState" ] (D.map playerStateFromString D.string))


port joinRoom : String -> Cmd msg


port sendData : E.Value -> Cmd msg


port dataReceiver : (E.Value -> msg) -> Sub msg


port playerCurrentTimeReceiver : (Float -> msg) -> Sub msg


port joinedRoom : (Int -> msg) -> Sub msg


port emitPlayerMsg : E.Value -> Cmd msg


sendPlayerMessage : Player.Msg -> Cmd msg
sendPlayerMessage playerMsg =
    emitPlayerMsg (Player.encodePlayerMsg playerMsg)


port playerMsgReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ dataReceiver ReceiveData
        , playerMsgReceiver ReceivePlayerMsg
        , playerCurrentTimeReceiver ReceiveCurrentTimeForSync
        , joinedRoom JoinedRoom
        ]


updateModelRoom : Room -> Model -> Model
updateModelRoom newRoom oldModel =
    case oldModel of
        Loaded loadedModel ->
            Loaded { loadedModel | room = newRoom }

        _ ->
            oldModel


updateModelInputText : String -> Model -> Model
updateModelInputText newInput oldModel =
    case oldModel of
        Loaded loadedModel ->
            Loaded { loadedModel | inputText = newInput }

        _ ->
            oldModel
