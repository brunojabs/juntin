module LinkGenerator exposing (generate)

import Random
import Random.Char exposing (english)
import Random.String exposing (string)


generate : (String -> msg) -> Cmd msg
generate msg =
    Random.generate msg (string 10 english)
