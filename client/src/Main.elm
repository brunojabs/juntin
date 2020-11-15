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


type Page
    = Home Home.Model
    | Room Room.Model
    | NotFound


type alias Config =
    { youtubeAPIKey : String }


type alias Model =
    { page : Page
    , key : Key
    , config : Config
    }


init : Config -> Url -> Key -> ( Model, Cmd Msg )
init config url key =
    changeToRoute config url key



---- UPDATE ----


type Msg
    = GotRoomMsg Room.Msg
    | GotHomeMsg Home.Msg
    | UrlChanged Url
    | UrlRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        roomUpdate =
            Room.update model.config.youtubeAPIKey
    in
    case ( msg, model.page ) of
        ( GotRoomMsg roomMsg, Room room ) ->
            roomUpdate roomMsg room |> updateWith Room GotRoomMsg model

        ( GotHomeMsg homeMsg, Home home ) ->
            Home.update homeMsg home |> updateWith Home GotHomeMsg model

        ( UrlChanged url, _ ) ->
            changeToRoute model.config url model.key

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | page = toModel subModel }
    , Cmd.map toMsg subCmd
    )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    let
        body =
            case model.page of
                Room room ->
                    [ Html.map GotRoomMsg (Room.view room) ]

                Home _ ->
                    [ Html.map GotHomeMsg Home.view ]

                NotFound ->
                    [ text "Não encontrado" ]
    in
    { title = "Juntin"
    , body = body
    }


changeToRoute : Config -> Url -> Key -> ( Model, Cmd Msg )
changeToRoute config url key =
    case Route.parseRoute url of
        Route.Room roomID ->
            let
                ( roomModel, roomMsg ) =
                    Room.init roomID config.youtubeAPIKey
            in
            updateWith Room GotRoomMsg { config = config, key = key, page = Room roomModel } ( roomModel, roomMsg )

        Route.Root ->
            let
                ( homeModel, homeMsg ) =
                    Home.init key
            in
            updateWith Home GotHomeMsg { config = config, key = key, page = Home homeModel } ( homeModel, homeMsg )

        _ ->
            ( { config = config, key = key, page = NotFound }, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Room room ->
            Sub.map GotRoomMsg (Room.subscriptions room)

        _ ->
            Sub.none



---- PROGRAM ----


main : Program Config Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> UrlRequested
        , onUrlChange = UrlChanged
        }
