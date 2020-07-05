module Route exposing (..)

import Room exposing (RoomID)
import Url exposing (Url)
import Url.Parser as UrlParser


type Route
    = Root
    | Room RoomID
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
