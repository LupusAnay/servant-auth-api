module Data.Session exposing (..)

import Browser.Navigation as Nav
import Data.User exposing (UserId)
import Json.Decode exposing (Decoder, field, int, map3)


type Session
    = LoggedIn Nav.Key JWTData
    | Guest Nav.Key


type alias JWTData =
    { created : Int, expires : Int, userId : UserId }


jwtToSession : Maybe JWTData -> Nav.Key -> Session
jwtToSession maybeSession key =
    case maybeSession of
        Just session ->
            LoggedIn key session

        Nothing ->
            Guest key


jwtDataDecoder : Decoder JWTData
jwtDataDecoder =
    map3 JWTData (field "created" int) (field "expires" int) (field "userId" int)


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key
