module Pages.NotFound exposing (..)

import Data.Session exposing (Session)
import Element exposing (Element, text)


type alias Msg =
    ()


type alias Model =
    Session


view : Model -> Element Msg
view model =
    text "NotFound"


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
