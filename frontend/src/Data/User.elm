module Data.User exposing (..)


type alias UserId =
    Int


type alias User =
    { userId : UserId, username : String, email : String }
