module LinkGenerator exposing (generate)

import Random
import Random.Char exposing (ascii)
import Random.String exposing (string)


generate : (String -> msg) -> Cmd msg
generate msg =
    Random.generate msg (string 10 ascii)
