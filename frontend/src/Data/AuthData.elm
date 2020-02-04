module Data.AuthData exposing (..)

import Json.Encode as Encode


type alias AuthData =
    { username : String, password : String }


authDataEncoder : AuthData -> Encode.Value
authDataEncoder data =
    Encode.object [ ( "username", Encode.string data.username ), ( "password", Encode.string data.password ) ]
