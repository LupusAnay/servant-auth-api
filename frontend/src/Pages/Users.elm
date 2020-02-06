module Pages.Users exposing (..)

import Data.Session exposing (Session)
import Element exposing (Element, text)


type alias Msg =
    ()


type alias Model =
    Session


view : Model -> Element Msg
view model =
    text "Users"


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )
