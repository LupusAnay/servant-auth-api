module Utils exposing (..)

import Http


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
