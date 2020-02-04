module Data.LoginForm exposing (..)


type alias LoginForm =
    { username : String, password : String }


emptyLoginForm : LoginForm
emptyLoginForm =
    LoginForm "" ""
