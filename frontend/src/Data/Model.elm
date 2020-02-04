module Data.Model exposing (..)

import Data.AuthSession exposing (Session)
import Data.LoginForm exposing (LoginForm)


type Model
    = Loading
    | LoggedIn Session
    | LoggedOut LoginForm
    | Error String
