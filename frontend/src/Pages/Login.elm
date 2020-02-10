module Pages.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Http exposing (jsonBody)
import Json.Encode as Encode
import Session exposing (Session, sessionDecoder)
import Utils exposing (RemoteData(..), WebData, checkSession, fromResult, post, updateForm)
import Views exposing (currentPasswordInput, errorsView, sendButtonView, unprotectedHeaderView, usernameInput)


type alias LoginForm =
    { username : String, password : String }


type alias Model =
    { toggle : Bool, form : LoginForm, errors : List String }


init : ( Model, Cmd Msg )
init =
    ( { toggle = False, form = { username = "", password = "" }, errors = [] }, checkSession GotSession )


loginFormEncoder : LoginForm -> Encode.Value
loginFormEncoder data =
    Encode.object [ ( "username", Encode.string data.username ), ( "password", Encode.string data.password ) ]


login : LoginForm -> Cmd Msg
login data =
    post
        { path = "sessions"
        , body = jsonBody <| loginFormEncoder data
        , expect = Http.expectJson (fromResult >> GotServerResponse) sessionDecoder
        }


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        UsernameChanged username ->
            ( { model | form = updateForm (\form -> { form | username = username }) model.form }, Cmd.none )

        PasswordChanged password ->
            ( { model | form = updateForm (\form -> { form | password = password }) model.form }, Cmd.none )

        LoginPressed ->
            ( model, login model.form )

        GotServerResponse result ->
            case result of
                Success _ ->
                    ( model, Nav.pushUrl key "users" )

                Failure error ->
                    ( { model | errors = Debug.toString error :: model.errors }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotSession webData ->
            case webData of
                Success _ ->
                    ( { model | toggle = False }, Nav.pushUrl key "users" )

                _ ->
                    ( { model | toggle = True }, Cmd.none )


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | LoginPressed
    | GotServerResponse (WebData Session)
    | GotSession (WebData Session)


loginForm : LoginForm -> Element Msg
loginForm form =
    column [ spacing 10, centerX, centerY ]
        [ usernameInput form.username UsernameChanged
        , currentPasswordInput form.password PasswordChanged
        , sendButtonView "Login" (Just LoginPressed)
        , row
            [ spacing 5 ]
            [ text "Don't have an account?"
            , link [ Font.color <| Element.rgb255 112 97 255 ] { url = "/registration", label = text "Registration" }
            ]
        ]


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ unprotectedHeaderView
        , errorsView model.errors
        , if model.toggle then
            loginForm model.form

          else
            none
        ]
