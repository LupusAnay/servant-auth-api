module Main exposing (main)

import Api.Msg as Api exposing (Msg)
import Api.Sessions exposing (HttpResult, login)
import Browser
import Data.AuthData exposing (AuthData)
import Data.AuthSession exposing (Session)
import Html exposing (..)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type Model
    = Loading
    | LoggedIn Session
    | LoggedOut
    | Error String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, login <| AuthData "Fucker_3" "fuck" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Api.GotSession session ->
            ( LoggedIn session, Cmd.none )

        Api.Error err ->
            ( Error <| Debug.toString err, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        LoggedIn session ->
            text <| Debug.toString session

        Error err ->
            text err

        Loading ->
            text "Loading"

        LoggedOut ->
            text "Logged Out"
