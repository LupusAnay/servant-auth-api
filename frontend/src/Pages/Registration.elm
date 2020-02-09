module Pages.Registration exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Session exposing (Session)
type alias Model = ()
type alias Msg = ()

init : Maybe Session -> (Model, Cmd Msg)
init session = ((), Cmd.none)

view : Model -> Element Msg
view model = text "Registration"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = ((), Cmd.none)

