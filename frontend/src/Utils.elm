module Utils exposing (..)

import Http exposing (Body, Expect, emptyBody)
import Session exposing (Session, sessionDecoder)


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
    "http://localhost:8080/" ++ path


checkSession : (WebData Session -> msg) -> Cmd msg
checkSession msg =
    get { path = "sessions", expect = Http.expectJson (fromResult >> msg) sessionDecoder }


logout : (Result Http.Error () -> msg) -> Cmd msg
logout msg =
    delete { path = "sessions", expect = Http.expectWhatever msg }


get : { path : String, expect : Expect msg } -> Cmd msg
get config =
    Http.riskyRequest
        { method = "GET"
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


delete : { path : String, expect : Expect msg } -> Cmd msg
delete config =
    Http.riskyRequest
        { method = "DELETE"
        , url = apiUrl config.path
        , body = emptyBody
        , expect = config.expect
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }
