module Data.LoginForm exposing (..)


type alias LoginForm =
    { username : String, password : String }


emptyLoginForm : LoginForm
emptyLoginForm =
    LoginForm "" ""


setUsername : String -> LoginForm -> LoginForm
setUsername name form =
    { form | username = name }


setPassword : String -> LoginForm -> LoginForm
setPassword password form =
    { form | password = password }
