module Pages.Registration exposing (..)

import Data.Session exposing (Session)
import Element exposing (Element, text)


type alias Msg =
    ()


type alias Model =
    Session


view : Model -> Element Msg
view model =
    text "Registration"


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
