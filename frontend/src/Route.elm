module Route exposing (Route(..), parseUrl)

import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Login
    | Logout
    | Users
    | Registration
    | NotFound
    | Index


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Login (s "login")
        , map Users (s "users")
        , map Registration (s "registration")
        , map Index top
        , map Logout (s "logout")
        ]
