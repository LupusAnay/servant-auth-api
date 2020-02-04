module Data.RegistrationForm exposing (..)


type alias RegistrationForm =
    { username : String, email : String, password : String, passwordConfirmation : String }


emptyRegistrationForm : RegistrationForm
emptyRegistrationForm =
    RegistrationForm "" "" "" ""
