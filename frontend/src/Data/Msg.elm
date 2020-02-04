module Data.Msg exposing (..)

import Data.AuthSession exposing (Session)


type Msg
    = GotSession Session
    | ChangeUsername String
    | ChangePassword String
    | PressLogin
    | GotError String
