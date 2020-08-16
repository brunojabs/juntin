port module Room exposing (Model, Msg, RoomID, init, subscriptions, update, view)

import Array
import ControlButton exposing (pauseButtonView, playButtonView)
import Html exposing (Html, button, div, form, h3, img, input, label, li, text, ul)
import Html.Attributes exposing (attribute, class, for, hidden, id, name, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import Logo exposing (logo, logoLoading)
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
    = AddVideo
    | TextChanged String
    | JoinedRoom Int
    | ReceiveData E.Value
    | ReceivePlayerMsg String
    | ReceiveCurrentTimeForSync Float
    | PlayVideo
    | PauseVideo
    | RemoveVideo Int


type BroadcastMsg
    = AskForData
    | SendData Room
    | Play
    | Pause
    | SendPlaylistItem String


type alias Room =
    { currentTime : Float
    , playerState : Player.State
    , playlist : List String
    , currentVideoIndex : Int
    }


initialModel : RoomID -> Model
initialModel roomID =
    Loaded
        { inputText = ""
        , room = initialRoom
        , roomID = roomID
        }


initialRoom =
    { currentTime = 0
    , playerState = Player.Unstarted
    , playlist = []
    , currentVideoIndex = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Joining roomID, JoinedRoom roomSize ) ->
            if roomSize == 1 then
                ( initialModel roomID
                , initPlayer ()
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

                        newModel =
                            updateModelRoom room (initialModel roomID)
                    in
                    case currentVideoID room of
                        Just videoID ->
                            ( newModel
                            , sendPlayerMessage (playCommand videoID room.currentTime)
                            )

                        Nothing ->
                            ( newModel, Cmd.none )

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

                Ok (SendPlaylistItem videoID) ->
                    let
                        cmd =
                            if List.isEmpty room.playlist then
                                sendPlayerMessage (Player.LoadVideo videoID 0)

                            else
                                Cmd.none
                    in
                    ( updateModelRoom { room | playlist = videoID :: room.playlist } model
                    , cmd
                    )

                Ok _ ->
                    ( model, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        ( Loaded _, TextChanged text ) ->
            ( updateModelInputText text model, Cmd.none )

        ( Loaded { inputText, room, roomID }, AddVideo ) ->
            case Player.youtubeIdFromUrl inputText of
                Just videoID ->
                    let
                        cmds =
                            if List.isEmpty room.playlist then
                                Cmd.batch
                                    [ sendPlayerMessage (Player.LoadVideo videoID 0)
                                    , sendData (encode roomID (SendPlaylistItem videoID))
                                    ]

                            else
                                sendData (encode roomID (SendPlaylistItem videoID))
                    in
                    ( model
                        |> updateModelInputText ""
                        |> updateModelRoom { room | playlist = room.playlist ++ [ videoID ] }
                    , cmds
                    )

                Nothing ->
                    ( model, Cmd.none )

        ( Loaded { room, roomID }, ReceivePlayerMsg playerMsg ) ->
            let
                newPlayerState =
                    playerStateFromString playerMsg

                newRoom =
                    { room | playerState = newPlayerState }

                newModel =
                    updateModelRoom newRoom model

                nextVideoIndex =
                    room.currentVideoIndex + 1
            in
            case newPlayerState of
                Ended ->
                    case getVideoAt nextVideoIndex room of
                        Just newVideoID ->
                            ( updateModelRoom { newRoom | currentVideoIndex = nextVideoIndex } newModel
                            , sendPlayerMessage <| Player.LoadVideo newVideoID 0
                            )

                        Nothing ->
                            ( newModel
                            , Cmd.none
                            )

                _ ->
                    ( newModel, Cmd.none )

        ( Loaded { roomID, room }, ReceiveCurrentTimeForSync newCurrentTime ) ->
            let
                newRoom =
                    { room | currentTime = newCurrentTime }
            in
            ( updateModelRoom newRoom model, sendData (encode roomID (SendData newRoom)) )

        ( Loaded { room, roomID }, PlayVideo ) ->
            ( model
            , Cmd.batch
                [ sendPlayerMessage Player.Play
                , sendData <| encode roomID Play
                ]
            )

        ( Loaded { room, roomID }, PauseVideo ) ->
            ( model
            , Cmd.batch
                [ sendPlayerMessage Player.Pause
                , sendData <| encode roomID Pause
                ]
            )

        ( Loaded { room }, RemoveVideo videoIndex ) ->
            ( updateModelRoom { room | playlist = removeVideoAt videoIndex room.playlist } model
            , Cmd.none
            )

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
                    [ div [] [ text ("Entrando na sala: " ++ roomID) ] ]

                WaitingSync roomID ->
                    [ div [] [ text ("Aguardando infos da sala: " ++ roomID) ] ]

                Loaded loadedModel ->
                    playerView loadedModel

        hidePlayer =
            case model of
                Loaded _ ->
                    False

                _ ->
                    True
    in
    div [ id "room", class "room" ]
        [ logoView model
        , div
            [ class "content__wrapper" ]
            content
        ]


controlButtonView : Model -> Html Msg
controlButtonView model =
    case model of
        Loaded { room } ->
            case room.playerState of
                Player.Playing ->
                    button [ onClick PauseVideo ] [ pauseButtonView ]

                _ ->
                    button [ onClick PlayVideo ] [ playButtonView ]

        _ ->
            div [] []


logoView : Model -> Html msg
logoView model =
    case model of
        WaitingSync _ ->
            logoLoading

        Joining _ ->
            logoLoading

        Loaded _ ->
            logo


playerView : LoadedModel -> List (Html Msg)
playerView loadedModel =
    [ playlistView loadedModel.room
    , div [ class "divider" ] []
    , div [ class "player__wrapper" ]
        [ div [ class "player-container" ]
            [ div [ id "player", attribute "data-plyr-provider" "youtube", attribute "data-plyr-embed-id" "hBCUuSr-0Nk" ]
                [ div [ class "remove" ] [], div [ class "remove" ] [], button [ class "remove" ] [] ]
            ]
        , formView loadedModel
        ]
    ]


formView : LoadedModel -> Html Msg
formView loadedModel =
    form
        [ onSubmit AddVideo, class "video-url" ]
        [ div
            [ class "video-url__group" ]
            [ input
                [ type_ "text"
                , placeholder "Cole o link para adicionar"
                , onInput TextChanged
                , value loadedModel.inputText
                , class "video-url__input"
                , name "video-url-input"
                , id "video-url-input"
                ]
                []
            , label
                [ class "video-url__label", for "video-url-input" ]
                [ text "Cole o link para adicionar" ]
            ]
        , button [ type_ "submit", class "button" ] [ text "+" ]
        ]


playlistView : Room -> Html Msg
playlistView room =
    div [ class "playlist" ]
        [ h3 [ class "playlist__title" ]
            [ text "Lista de reprodução" ]
        , ul
            [ class "playlist__list" ]
            (List.indexedMap (playlistItemView room.currentVideoIndex) room.playlist)
        ]


playlistItemView : Int -> Int -> String -> Html Msg
playlistItemView currentVideoIndex index videoID =
    let
        ( currentClass, removeButton ) =
            if currentVideoIndex == index then
                ( "playlist__current_item", [] )

            else
                ( ""
                , [ playlistRemoveView index ]
                )
    in
    li [ class currentClass ] ([ text videoID ] ++ removeButton)


playlistRemoveView index =
    button
        [ onClick (RemoveVideo index)
        , class "button playlist__remove-button"
        ]
        [ text "[x]" ]


encode : RoomID -> BroadcastMsg -> E.Value
encode roomID broadcastMsg =
    case broadcastMsg of
        SendData room ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "playerState", E.string (stringFromPlayerState room.playerState) )
                        , ( "currentTime", E.float room.currentTime )
                        , ( "playlist", E.list E.string room.playlist )
                        , ( "currentVideoIndex", E.int room.currentVideoIndex )
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

        SendPlaylistItem videoID ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "videoID", E.string videoID )
                        ]
                  )
                , ( "roomID", E.string roomID )
                , ( "message", E.string "SendPlaylistItem" )
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

        "SendPlaylistItem" ->
            D.at [ "data", "videoID" ] D.string |> D.map SendPlaylistItem

        "SendData" ->
            roomDecoder |> D.map SendData

        _ ->
            D.fail ":("


roomDecoder : D.Decoder Room
roomDecoder =
    D.map4 Room
        (D.at [ "data", "currentTime" ] D.float)
        (D.at [ "data", "playerState" ] (D.map playerStateFromString D.string))
        (D.at [ "data", "playlist" ] (D.list D.string))
        (D.at [ "data", "currentVideoIndex" ] D.int)


port joinRoom : String -> Cmd msg


port sendData : E.Value -> Cmd msg


port dataReceiver : (E.Value -> msg) -> Sub msg


port playerCurrentTimeReceiver : (Float -> msg) -> Sub msg


port joinedRoom : (Int -> msg) -> Sub msg


port emitPlayerMsg : E.Value -> Cmd msg


port initPlayer : () -> Cmd msg


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


getVideoAt : Int -> Room -> Maybe String
getVideoAt index room =
    Array.get index (Array.fromList room.playlist)


currentVideoID : Room -> Maybe String
currentVideoID room =
    Array.get room.currentVideoIndex (Array.fromList room.playlist)


removeVideoAt : Int -> List String -> List String
removeVideoAt index playlist =
    case List.drop index playlist of
        [] ->
            playlist

        _ :: rest ->
            List.take index playlist ++ rest
