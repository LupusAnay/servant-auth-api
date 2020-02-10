module Pages.Index exposing (..)

import Browser.Navigation as Nav
import Element exposing (Element, centerX, centerY, el, text)
import Element.Font as Font
import Http
import Session exposing (Session, sessionDecoder)
import Url exposing (Protocol(..))
import Utils exposing (RemoteData(..), WebData, fromResult, get)


type alias Model =
    { session : Maybe Session }


type Msg
    = GotServerResponse (WebData Session)


init : Maybe Session -> ( Model, Cmd Msg )
init sess =
    ( { session = sess }, getSession )


view : Model -> Element Msg
view model =
    el [centerX, centerY, Font.size 20] <| text "Loading"


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        GotServerResponse webData ->
            case webData of
                Success sess ->
                    ( model, Nav.pushUrl key "users" )

                _ ->
                    ( model, Nav.pushUrl key "login" )


getSession : Cmd Msg
getSession =
    get { path = "sessions", expect = Http.expectJson (fromResult >> GotServerResponse) sessionDecoder }
