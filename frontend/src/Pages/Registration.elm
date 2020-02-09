module Pages.Registration exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (..)
import Http exposing (jsonBody)
import Json.Encode as Encode
import Session exposing (Session)
import User exposing (UserId, userIdDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult, post, updateForm)


type alias Model =
    { session : Maybe Session, form : RegistrationForm, errors : List String }


type Msg
    = UsernameChanged String
    | EmailChanged String
    | PasswordChanged String
    | PasswordConfirmationChanged String
    | RegistrationPressed
    | GotServerResponse (WebData UserId)


type alias RegistrationForm =
    { username : String, email : String, password : String, passwordConfirmation : String }


emptyRegistrationForm : RegistrationForm
emptyRegistrationForm =
    RegistrationForm "" "" "" ""


init : Maybe Session -> ( Model, Cmd Msg )
init session =
    ( { session = session, form = emptyRegistrationForm, errors = [] }, Cmd.none )


view : Model -> Element Msg
view model =
    registrationFormView model.form


registrationFormView : RegistrationForm -> Element Msg
registrationFormView form =
    column [ padding 5, spacing 5, centerX, centerY ]
        [ username []
            { text = form.username
            , placeholder = Just <| placeholder [] <| Element.text "Username"
            , label = labelHidden "username"
            , onChange = UsernameChanged
            }
        , email
            []
            { text = form.email
            , placeholder = Just <| placeholder [] <| Element.text "Email"
            , label = labelHidden "email"
            , onChange = EmailChanged
            }
        , newPassword []
            { onChange = PasswordChanged
            , text = form.password
            , label = labelHidden "password"
            , show = False
            , placeholder = Just <| placeholder [] <| Element.text "Password"
            }
        , newPassword []
            { onChange = PasswordConfirmationChanged
            , text = form.passwordConfirmation
            , label = labelHidden "passwordConfirmation"
            , show = False
            , placeholder = Just <| placeholder [] <| Element.text "Password Again"
            }
        , button
            [ Background.color <| Element.rgb255 251 249 255
            , Border.rounded 3
            , Border.color <| Element.rgb255 0 0 0
            , Border.width <| 1
            , width fill
            , height <| px 50
            ]
            { onPress = Just RegistrationPressed
            , label = Element.el [ padding 10 ] <| Element.text "Create Account"
            }
        , row
            [ spacing 5 ]
            [ Element.text "Already have an account?"
            , link [ Font.color <| Element.rgb255 112 97 255 ] { url = "/login", label = Element.text "Log In" }
            ]
        ]


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update key msg model =
    case msg of
        UsernameChanged username ->
            ( { model | form = updateForm (\f -> { f | username = username }) model.form }, Cmd.none )

        EmailChanged email ->
            ( { model | form = updateForm (\form -> { form | email = email }) model.form }, Cmd.none )

        PasswordChanged password ->
            ( { model | form = updateForm (\form -> { form | password = password }) model.form }, Cmd.none )

        PasswordConfirmationChanged passwordConfirmation ->
            ( { model | form = updateForm (\form -> { form | passwordConfirmation = passwordConfirmation }) model.form }, Cmd.none )

        RegistrationPressed ->
            ( model, register model.form )

        GotServerResponse result ->
            case result of
                Success userId ->
                    ( model, Nav.pushUrl key "login" )

                Failure error ->
                    ( { model | errors = Debug.toString error :: model.errors }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


registrationFormEncoder : RegistrationForm -> Encode.Value
registrationFormEncoder form =
    Encode.object [ ( "username", Encode.string form.username ), ( "email", Encode.string form.email ), ( "password", Encode.string form.password ) ]


register : RegistrationForm -> Cmd Msg
register form =
    post
        { path = "users"
        , body = jsonBody <| registrationFormEncoder form
        , expect = Http.expectJson (fromResult >> GotServerResponse) userIdDecoder
        }
