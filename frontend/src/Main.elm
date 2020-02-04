module Main exposing (main)

import Browser
import Data.AuthSession exposing (Session)
import Html exposing (..)
import SessionsApi exposing (HttpResult, Msg(..), login)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type alias Model =
    Result String Session


init : () -> ( Model, Cmd Msg )
init _ =
    ( Err "Loading", login )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GotSession response ->
            case response of
                Ok session ->
                    ( Ok session, Cmd.none )

                Err err ->
                    ( Err <| Debug.toString err, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        Ok sess ->
            text <| Debug.toString sess

        Err err ->
            text err
