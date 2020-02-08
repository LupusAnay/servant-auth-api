module Pages.Login exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button, currentPassword, labelHidden, username)
import Http exposing (jsonBody)
import Json.Encode as Encode
import Session exposing (Session, sessionDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult)


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
        , url = "http://localhost:8080/sessions"
        , body = jsonBody <| loginFormEncoder data
        , expect = Http.expectJson (fromResult >> GotServerResponse) sessionDecoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }


updateForm : (LoginForm -> LoginForm) -> LoginForm -> LoginForm
updateForm setter form =
    setter form


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                    ( { model | session = Just session }, Cmd.none )

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
    column [ padding 5, spacing 5 ]
        [ username []
            { text = form.username
            , placeholder = Nothing
            , label = labelHidden "username"
            , onChange = UsernameChanged
            }
        , currentPassword []
            { onChange = PasswordChanged
            , text = form.password
            , label = labelHidden "password"
            , show = False
            , placeholder = Nothing
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
        ]


view : Model -> Element Msg
view model =
    loginForm model.form
