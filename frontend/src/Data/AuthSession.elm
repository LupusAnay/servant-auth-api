module Data.AuthSession exposing (..)

import Data.User exposing (UserId)


type alias Session =
    { created : Int, expires : Int, userId : UserId }
