module Data.AuthSession exposing (..)

import Data.User exposing (UserId)
import Json.Decode exposing (Decoder, field, int, map3)


type alias Session =
    { created : Int, expires : Int, userId : UserId }


sessionDecoder : Decoder Session
sessionDecoder =
    map3 Session (field "created" int) (field "expires" int) (field "userId" int)
