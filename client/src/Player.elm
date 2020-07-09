module Player exposing (..)

import Json.Decode as D
import Json.Encode as E


type State
    = Unstarted
    | Ended
    | Playing
    | Paused
    | Buffering
    | VideoCued
    | Unknown


type Msg
    = Play
    | Pause
    | LoadVideo String Float
    | CueVideo String Float
    | GetCurrentTime


playerStateFromString : String -> State
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


stringFromPlayerState : State -> String
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


playMsgToString : Msg -> String
playMsgToString msg =
    case msg of
        Play ->
            "play"

        Pause ->
            "pause"

        LoadVideo _ _ ->
            "loadVideo"

        CueVideo _ _ ->
            "cueVideo"

        GetCurrentTime ->
            "getCurrentTime"


encodePlayerMsg : Msg -> E.Value
encodePlayerMsg playerMsg =
    case playerMsg of
        Play ->
            E.object [ ( "message", E.string "play" ) ]

        Pause ->
            E.object [ ( "message", E.string "pause" ) ]

        GetCurrentTime ->
            E.object [ ( "message", E.string "getCurrentTime" ) ]

        LoadVideo videoID time ->
            E.object
                [ ( "message", E.string "loadVideo" )
                , ( "data", E.object [ ( "videoID", E.string videoID ), ( "time", E.float time ) ] )
                ]

        CueVideo videoID time ->
            E.object
                [ ( "message", E.string "cueVideo" )
                , ( "data", E.object [ ( "videoID", E.string videoID ), ( "time", E.float time ) ] )
                ]
