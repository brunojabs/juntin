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


type alias PlayerMsgPayload =
    { message : PlayerMsg
    , data : String
    }


init : ( Model, Cmd Msg )
init =
    ( { paused = True, text = "" }, Cmd.batch [ joinRoom "Sala2", sendPlayerMessage (LoadVideo "hBCUuSr-0Nk") ] )


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


encodePlayerMsg : PlayerMsgPayload -> E.Value
encodePlayerMsg msgPayload =
    E.object
        [ ( "message", E.string (playMsgToString msgPayload.message) )
        , ( "data", E.string msgPayload.data )
        ]


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



---- UPDATE ----


type Msg
    = Join
    | SendData
    | ReceiveData E.Value
    | ReceivePlayerMsg String
    | TextChanged String
    | SetVideo


type PlayerMsg
    = Play
    | Pause
    | LoadVideo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendData ->
            ( model, sendData (encode model) )

        Join ->
            ( model, joinRoom "Sala2" )

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
                    if newModel.paused then
                        ( newModel, Cmd.batch (sendPlayerMessage Pause :: msgs) )

                    else
                        ( newModel, Cmd.batch (sendPlayerMessage Play :: msgs) )

                Err e ->
                    ( model, Cmd.none )

        ReceivePlayerMsg playerMsg ->
            if playerMsg == "playing" then
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

            else if playerMsg == "paused" then
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

            else
                ( model, Cmd.none )



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
