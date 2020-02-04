module Api.Msg exposing (..)

import Data.AuthSession exposing (Session)


type Msg
    = GotSession Session
    | Error String
