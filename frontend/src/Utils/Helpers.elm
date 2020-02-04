module Utils.Helpers exposing (..)

import Api.Msg exposing (Msg(..))


liftResult : (right -> Msg) -> Result left right -> Msg
liftResult f res =
    case res of
        Ok ok ->
            f ok

        Err err ->
            Error <| Debug.toString err
