module Session exposing (Session, sessionDecoder)

import Json.Decode as Decode exposing (Decoder, int)
import Json.Decode.Pipeline exposing (required)
import User exposing (UserId, userIdDecoder)


type alias Session =
    { created : Int, expires : Int, userId : UserId }


sessionDecoder : Decoder Session
sessionDecoder =
    Decode.succeed Session |> required "created" int |> required "expires" int |> required "userId" userIdDecoder
