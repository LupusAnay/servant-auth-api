module Pages.Login exposing (..)

import Api.Sessions exposing (login)
import Data.LoginForm exposing (LoginForm, setPassword, setUsername)
import Data.Session as Session exposing (JWTData, Session, jwtToSession)
import Element exposing (Element, column, fill, height, padding, px, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input exposing (button, currentPassword, labelHidden, username)
import Http
import Route exposing (Route(..))


type alias Model =
    { session : Session, form : LoginForm, errors : List String }


type Msg
    = GotSession Session
    | ChangeUsername String
    | ChangePassword String
    | PressLogin
    | GotError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSession session ->
            ( { model | session = session }, Route.replaceUrl (Session.navKey session) Route.Users )

        ChangeUsername username ->
            ( { model | form = setUsername username model.form }, Cmd.none )

        ChangePassword password ->
            ( { model | form = setPassword password model.form }, Cmd.none )

        PressLogin ->
            ( model
            , login model.form <|
                \data ->
                    case data of
                        Ok jwt ->
                            GotSession <| jwtToSession (Just jwt) (Session.navKey model.session)

                        Err err ->
                            GotError <| Debug.toString err
            )

        GotError error ->
            ( { model | errors = error :: model.errors }, Cmd.none )


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


view : Model -> Element Msg
view model =
    loginForm model.form
