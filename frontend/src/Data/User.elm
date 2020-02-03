module Data.User exposing (..)


type alias UserId
    = Int


type alias User =
    { userId : UserId, username : String, email : String }


type alias AuthData =
    { username : String, password : String }


type alias NewUser =
    { username : String, email : String, password : String }
