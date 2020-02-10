module Pages.Registration exposing (Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Http exposing (jsonBody)
import Json.Encode as Encode
import Session exposing (Session)
import User exposing (UserId, userIdDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult, post, updateForm)
import Views exposing (emailInput, errorsView, linkView, newPasswordInput, sendButtonView, unprotectedHeaderView, usernameInput)


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
    column [ width fill, height fill ] [ unprotectedHeaderView, registrationFormView model.form, errorsView model.errors ]


registrationFormView : RegistrationForm -> Element Msg
registrationFormView form =
    column [ spacing 10, centerX, centerY ]
        [ usernameInput form.username UsernameChanged
        , emailInput form.email EmailChanged
        , newPasswordInput form.password "Password" PasswordChanged
        , newPasswordInput form.passwordConfirmation "Password Again" PasswordConfirmationChanged
        , sendButtonView "Register" (Just RegistrationPressed)
        , row
            [ spacing 5 ]
            [ Element.text "Already have an account?"
            , linkView "/login" "Log In"
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
            ( { model | errors = [] }, register model.form )

        GotServerResponse result ->
            case result of
                Success _ ->
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
