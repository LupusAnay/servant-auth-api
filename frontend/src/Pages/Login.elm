module Pages.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Http exposing (jsonBody)
import Json.Encode as Encode
import Session exposing (Session, sessionDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult, post, updateForm)
import Views exposing (currentPasswordInput, errorsView, sendButtonView, unprotectedHeaderView, usernameInput)


type alias LoginForm =
    { username : String, password : String }


type alias Model =
    { session : Maybe Session, form : LoginForm, errors : List String }


init : ( Model, Cmd Msg )
init =
    ( { session = Nothing, form = { username = "", password = "" }, errors = [] }, Cmd.none )


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
                Success session ->
                    ( { model | session = Just session }, Nav.pushUrl key "users" )

                Failure error ->
                    ( { model | session = Nothing, errors = Debug.toString error :: model.errors }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


type Msg
    = UsernameChanged String
    | PasswordChanged String
    | LoginPressed
    | GotServerResponse (WebData Session)


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
        , loginForm model.form
        ]
