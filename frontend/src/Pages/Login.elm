module Pages.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (Placeholder, button, currentPassword, labelHidden, placeholder, username)
import Http exposing (jsonBody)
import Json.Encode as Encode
import Session exposing (Session, sessionDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult, updateForm)


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
    Http.riskyRequest
        { method = "POST"
        , url = "http://109.167.191.151:8080/sessions"
        , body = jsonBody <| loginFormEncoder data
        , expect = Http.expectJson (fromResult >> GotServerResponse) sessionDecoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
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
    column [ spacing 10 ]
        [ username []
            { text = form.username
            , placeholder = Just <| placeholder [] <| text "Username"
            , label = labelHidden "username"
            , onChange = UsernameChanged
            }
        , currentPassword []
            { onChange = PasswordChanged
            , text = form.password
            , label = labelHidden "password"
            , show = False
            , placeholder = Just <| placeholder [] <| text "Password"
            }
        , button
            [ Background.color <| Element.rgb255 251 249 255
            , Border.rounded 3
            , Border.color <| Element.rgb255 0 0 0
            , Border.width <| 1
            , width fill
            , height <| px 50
            ]
            { onPress = Just LoginPressed
            , label = Element.el [ padding 10 ] <| text "Login"
            }
        , row
            [ spacing 5 ]
            [ text "Don't have an account?"
            , link [ Font.color <| Element.rgb255 112 97 255 ] { url = "/registration", label = text "Registration" }
            ]
        ]


view : Model -> Element Msg
view model =
    column [ centerX, centerY ]
        [ column [ Font.color <| Element.rgb255 255 1 0, padding 5 ] <| List.map text model.errors
        , loginForm model.form
        ]
