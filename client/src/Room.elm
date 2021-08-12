port module Room exposing (Model, Msg, RoomID, init, subscriptions, update, view)

import Array
import ControlButton exposing (pauseButtonView, playButtonView)
import Html exposing (Html, button, div, form, h3, img, input, label, li, text, ul)
import Html.Attributes exposing (class, for, hidden, id, name, placeholder, src, type_, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)
import Http
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
    | SkipToVideo Int
    | ChangeVolume String
    | GotVideoInfo (Result Http.Error Video)


type BroadcastMsg
    = AskForData
    | SendData Room
    | Play
    | Pause
    | SendPlaylistItem Video
    | SkipToPlaylistItem Int
    | RemovePlaylistItem Int


type alias Room =
    { currentTime : Float
    , playerState : Player.State
    , playlist : List Video
    , currentVideoIndex : Int
    , volume : String
    }


type alias Video =
    { id : String
    , title : String
    , thumbnail : String
    }


videoDecoder : D.Decoder Video
videoDecoder =
    D.map3 Video
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "thumbnail" D.string)


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
    , volume = "100"
    }


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update youtubeAPIKey msg model =
    case ( model, msg ) of
        ( Joining roomID, JoinedRoom roomSize ) ->
            if roomSize == 1 then
                ( initialModel roomID
                , sendPlayerMessage (Player.CueVideo "hBCUuSr-0Nk" 0)
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
                        Just video ->
                            ( newModel
                            , sendPlayerMessage (playCommand video.id room.currentTime)
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

                Ok (SendPlaylistItem video) ->
                    let
                        cmd =
                            if List.isEmpty room.playlist then
                                sendPlayerMessage (Player.LoadVideo video.id 0)

                            else
                                Cmd.none
                    in
                    ( updateModelRoom { room | playlist = room.playlist ++ [ video ] } model
                    , cmd
                    )

                Ok (SkipToPlaylistItem videoIndex) ->
                    case getVideoAt videoIndex room of
                        Just newVideo ->
                            ( updateModelRoom { room | currentVideoIndex = videoIndex } model
                            , sendPlayerMessage <| Player.LoadVideo newVideo.id 0
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )

                Ok (RemovePlaylistItem videoIndex) ->
                    ( updateModelRoom { room | playlist = removeVideoAt videoIndex room.playlist } model
                    , Cmd.none
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
                    ( updateModelInputText "" model
                    , getVideoInfo videoID youtubeAPIKey
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
                        Just newVideo ->
                            ( updateModelRoom { newRoom | currentVideoIndex = nextVideoIndex } newModel
                            , sendPlayerMessage <| Player.LoadVideo newVideo.id 0
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

        ( Loaded { room, roomID }, RemoveVideo videoIndex ) ->
            ( updateModelRoom { room | playlist = removeVideoAt videoIndex room.playlist } model
            , sendData (encode roomID (RemovePlaylistItem videoIndex))
            )

        ( Loaded { room, roomID }, SkipToVideo videoIndex ) ->
            case getVideoAt videoIndex room of
                Just newVideo ->
                    ( updateModelRoom { room | currentVideoIndex = videoIndex } model
                    , Cmd.batch
                        [ sendPlayerMessage <| Player.LoadVideo newVideo.id 0
                        , sendData (encode roomID (SkipToPlaylistItem videoIndex))
                        ]
                    )

                Nothing ->
                    ( model
                    , Cmd.none
                    )

        ( Loaded { room }, ChangeVolume newVolume ) ->
            ( updateModelRoom { room | volume = newVolume } model
            , sendPlayerMessage (Player.SetVolume newVolume)
            )

        ( Loaded { room, roomID }, GotVideoInfo result ) ->
            case result of
                Ok video ->
                    let
                        cmds =
                            if List.isEmpty room.playlist then
                                Cmd.batch
                                    [ sendPlayerMessage (Player.LoadVideo video.id 0)
                                    , sendData (encode roomID (SendPlaylistItem video))
                                    ]

                            else
                                sendData (encode roomID (SendPlaylistItem video))
                    in
                    ( updateModelRoom { room | playlist = room.playlist ++ [ video ] } model, cmds )

                Err _ ->
                    -- TODO: Show an error message for the using saying something about the error
                    ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


init : RoomID -> String -> ( Model, Cmd Msg )
init roomID youtubeAPIKey =
    ( Joining roomID
    , joinRoom roomID
    )


view : Model -> Html Msg
view model =
    let
        content =
            case model of
                Joining roomID ->
                    div [] [ text ("Entrando na sala: " ++ roomID) ]

                WaitingSync roomID ->
                    div [] [ text ("Aguardando infos da sala: " ++ roomID) ]

                Loaded loadedModel ->
                    formView loadedModel

        hidePlayer =
            case model of
                Loaded _ ->
                    False

                _ ->
                    True
    in
    div [ class "room" ]
        [ logoView model
        , div
            [ class "content__wrapper" ]
            [ playlistView model
            , div [ class "divider" ] []
            , div [ class "player__wrapper" ]
                [ div [ id "player", hidden hidePlayer ]
                    []
                , controlButtonView model
                , volumeView model
                , content
                ]
            ]
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


playlistView : Model -> Html Msg
playlistView model =
    case model of
        Loaded { room } ->
            div [ class "playlist" ]
                [ h3 [ class "playlist__title" ]
                    [ text "Lista de reprodução" ]
                , ul
                    [ class "playlist__list" ]
                    (List.indexedMap (playlistItemView room.currentVideoIndex) room.playlist)
                ]

        _ ->
            div [] []


playlistItemView : Int -> Int -> Video -> Html Msg
playlistItemView currentVideoIndex index video =
    let
        ( currentClass, removeButton ) =
            if currentVideoIndex == index then
                ( "playlist__current_item", [] )

            else
                ( ""
                , [ playlistRemoveView index ]
                )
    in
    li
        [ class ("playlist__item " ++ currentClass)
        , onDoubleClick (SkipToVideo index)
        ]
        ([ img [ src video.thumbnail, class "playlist__thumbnail" ] []
         , text video.title
         ]
            ++ removeButton
        )


playlistRemoveView index =
    button
        [ onClick (RemoveVideo index)
        , class "button playlist__remove-button"
        ]
        [ text "[x]" ]


volumeView : Model -> Html Msg
volumeView model =
    case model of
        Loaded { room } ->
            div []
                [ input
                    [ type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "100"
                    , value <| room.volume
                    , onInput ChangeVolume
                    ]
                    []
                ]

        _ ->
            div [] []


videoEnconder : Video -> E.Value
videoEnconder video =
    E.object
        [ ( "id", E.string video.id )
        , ( "title", E.string video.title )
        , ( "thumbnail", E.string video.thumbnail )
        ]


encode : RoomID -> BroadcastMsg -> E.Value
encode roomID broadcastMsg =
    case broadcastMsg of
        SendData room ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "playerState", E.string (stringFromPlayerState room.playerState) )
                        , ( "currentTime", E.float room.currentTime )
                        , ( "playlist", E.list videoEnconder room.playlist )
                        , ( "currentVideoIndex", E.int room.currentVideoIndex )
                        , ( "volume", E.string "100" )
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

        SendPlaylistItem video ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "id", E.string video.id )
                        , ( "title", E.string video.title )
                        , ( "thumbnail", E.string video.thumbnail )
                        ]
                  )
                , ( "roomID", E.string roomID )
                , ( "message", E.string "SendPlaylistItem" )
                ]

        SkipToPlaylistItem itemIndex ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "itemIndex", E.int itemIndex )
                        ]
                  )
                , ( "roomID", E.string roomID )
                , ( "message", E.string "SkipToPlaylistItem" )
                ]

        RemovePlaylistItem itemIndex ->
            E.object
                [ ( "data"
                  , E.object
                        [ ( "itemIndex", E.int itemIndex )
                        ]
                  )
                , ( "roomID", E.string roomID )
                , ( "message", E.string "RemovePlaylistItem" )
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
            D.field "data" videoDecoder |> D.map SendPlaylistItem

        "SkipToPlaylistItem" ->
            D.at [ "data", "itemIndex" ] D.int |> D.map SkipToPlaylistItem

        "RemovePlaylistItem" ->
            D.at [ "data", "itemIndex" ] D.int |> D.map RemovePlaylistItem

        "SendData" ->
            roomDecoder |> D.map SendData

        _ ->
            D.fail ":("


roomDecoder : D.Decoder Room
roomDecoder =
    D.map5 Room
        (D.at [ "data", "currentTime" ] D.float)
        (D.at [ "data", "playerState" ] (D.map playerStateFromString D.string))
        (D.at [ "data", "playlist" ] (D.list videoDecoder))
        (D.at [ "data", "currentVideoIndex" ] D.int)
        (D.at [ "data", "volume" ] D.string)


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


getVideoAt : Int -> Room -> Maybe Video
getVideoAt index room =
    Array.get index (Array.fromList room.playlist)


currentVideoID : Room -> Maybe Video
currentVideoID room =
    Array.get room.currentVideoIndex (Array.fromList room.playlist)


removeVideoAt : Int -> List Video -> List Video
removeVideoAt index playlist =
    case List.drop index playlist of
        [] ->
            playlist

        _ :: rest ->
            List.take index playlist ++ rest


responseVideoDecoder : D.Decoder Video
responseVideoDecoder =
    D.map3 Video
        (D.field "items" (D.index 0 (D.field "id" D.string)))
        (D.field "items" (D.index 0 (D.at [ "snippet", "title" ] D.string)))
        (D.field "items" (D.index 0 (D.at [ "snippet", "thumbnails", "default", "url" ] D.string)))


getVideoInfo : String -> String -> Cmd Msg
getVideoInfo videoID youtubeAPIKey =
    Http.get
        { url = "https://www.googleapis.com/youtube/v3/videos?part=snippet&id=" ++ videoID ++ "&key=" ++ youtubeAPIKey
        , expect = Http.expectJson GotVideoInfo responseVideoDecoder
        }
