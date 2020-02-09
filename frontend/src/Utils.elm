module Utils exposing (..)

import Http exposing (Body, Expect, emptyBody)


type RemoteData error value
    = NotAsked
    | Loading
    | Success value
    | Failure error


fromResult : Result error value -> RemoteData error value
fromResult result =
    case result of
        Ok value ->
            Success value

        Err error ->
            Failure error


updateForm : (form -> form) -> form -> form
updateForm setter =
    setter


type alias WebData a =
    RemoteData Http.Error a


apiUrl path =
    "http://109.167.191.151:8080/" ++ path


get : { path : String, expect : Expect msg } -> Cmd msg
get config =
    Http.riskyRequest
        { method = "POST"
        , url = apiUrl config.path
        , body = emptyBody
        , expect = config.expect
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


post : { path : String, body : Body, expect : Expect msg } -> Cmd msg
post config =
    Http.riskyRequest
        { method = "POST"
        , url = apiUrl config.path
        , body = config.body
        , expect = config.expect
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }
