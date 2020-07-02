module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import LinkGenerator
import Room
import Url exposing (Url)
import Url.Parser as UrlParser



---- MODEL ----


type Model
    = Root
    | Room Room.Model
    | NotFound


type Route
    = RootRoute
    | RoomRoute Room.RoomID
    | NotFoundRoute


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map RootRoute UrlParser.top
        , UrlParser.map RoomRoute UrlParser.string
        ]


parseRoute : Url -> Route
parseRoute url =
    UrlParser.parse route url |> Maybe.withDefault NotFoundRoute


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url _ =
    case parseRoute url of
        RoomRoute roomID ->
            let
                ( roomModel, roomMsg ) =
                    Room.init roomID
            in
            updateWith Room GotRoomMsg (Room roomModel) ( roomModel, roomMsg )

        RootRoute ->
            ( Root, Cmd.none )

        _ ->
            ( NotFound, Cmd.none )


generateRoom : Cmd Msg
generateRoom =
    LinkGenerator.generate LinkGenerated



---- UPDATE ----


type Msg
    = LinkGenerated String
    | GotRoomMsg Room.Msg
    | UrlChanged
    | UrlRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkGenerated roomID, _ ) ->
            Room.init roomID |> updateWith Room GotRoomMsg model

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
                    [ text "Bem vindo ao Juntin" ]

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
