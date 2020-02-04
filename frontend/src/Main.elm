module Main exposing (main)

import Api.Sessions exposing (login)
import Browser
import Data.AuthData exposing (AuthData)
import Data.LoginForm exposing (LoginForm, emptyLoginForm)
import Data.Model exposing (Model(..))
import Data.Msg exposing (Msg(..))
import Element exposing (Element, column, fill, height, padding, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (Label, button, currentPassword, labelHidden, username)


main =
    Browser.element
        { init = init
        , update = update
        , view = \model -> Element.layout [ Element.centerX, Element.centerY ] <| view model
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( LoggedOut emptyLoginForm, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( LoggedIn session, Cmd.none )

        GotError err ->
            ( Error err, Cmd.none )

        ChangeUsername username ->
            case model of
                LoggedOut form ->
                    ( LoggedOut { form | username = username }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangePassword password ->
            case model of
                LoggedOut form ->
                    ( LoggedOut { form | password = password }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PressLogin ->
            case model of
                LoggedOut form ->
                    ( Loading, login <| AuthData form.username form.password )

                _ ->
                    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    case model of
        LoggedIn session ->
            text <| Debug.toString session

        Error err ->
            text err

        Loading ->
            text "Loading"

        LoggedOut from ->
            loginForm from


loginForm : LoginForm -> Element Msg
loginForm form =
    column [ padding 5, spacing 5 ]
        [ username []
            { text = form.username
            , placeholder = Nothing
            , label = labelHidden "username"
            , onChange = ChangeUsername
            }
        , currentPassword []
            { onChange = ChangePassword
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
            { onPress = Just PressLogin
            , label = Element.el [ padding 10 ] <| text "Login"
            }
        ]
