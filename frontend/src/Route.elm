module Route exposing (..)

import Browser.Navigation as Nav
import Url exposing (Url)


type Route
    = Login
    | Registration
    | Users
    | NotFound


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


urlToRoute : Url -> Route
urlToRoute url =
    case url.path of
        "login" ->
            Login

        "registration" ->
            Registration

        "users" ->
            Users

        _ ->
            NotFound


routeToString : Route -> String
routeToString route =
    "/" ++ String.join "/" (routeToPieces route)


routeToPieces : Route -> List String
routeToPieces route =
    case route of
        Login ->
            [ "login" ]

        Registration ->
            [ "registration" ]

        Users ->
            [ "users" ]

        NotFound ->
            [ "not-found" ]
