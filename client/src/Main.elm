module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Home
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LinkGenerator
import Room
import Route
import Url exposing (Url)
import Url.Parser as UrlParser



---- MODEL ----


type Model
    = Root
    | Room Room.Model
    | NotFound


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    case Route.parseRoute url of
        Route.Room roomID ->
            let
                ( roomModel, roomMsg ) =
                    Room.init roomID
            in
            updateWith Room GotRoomMsg (Room roomModel) ( roomModel, roomMsg )

        Route.Root ->
            ( Root, Cmd.none )

        _ ->
            ( NotFound, Cmd.none )



---- UPDATE ----


type Msg
    = GotRoomMsg Room.Msg
    | GotHomeMsg Home.Msg
    | UrlChanged
    | UrlRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotRoomMsg roomMsg, Room room ) ->
            Room.update roomMsg room |> updateWith Room GotRoomMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model of
                Room room ->
                    [ Html.map GotRoomMsg (Room.view room) ]

                Root ->
                    [ Html.map GotHomeMsg Home.view ]

                NotFound ->
                    [ text "Não encontrado" ]
    in
    { title = "Juntin"
    , body = body
    }



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Room room ->
            Sub.map GotRoomMsg (Room.subscriptions room)

        _ ->
            Sub.none



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
