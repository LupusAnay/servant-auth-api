module Views exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button, currentPassword, email, labelHidden, newPassword, placeholder, username)
import Session exposing (Session)


type alias OnChange msg =
    String -> msg


errorsView : List String -> Element msg
errorsView errors =
    column [ Font.color <| Element.rgb255 255 1 0, padding 5 ] <| List.map text errors


usernameInput : String -> OnChange msg -> Element msg
usernameInput text onChange =
    username []
        { text = text
        , placeholder = Just <| placeholder [] <| Element.text "Username"
        , label = labelHidden "username"
        , onChange = onChange
        }


currentPasswordInput : String -> OnChange msg -> Element msg
currentPasswordInput value onChange =
    currentPassword []
        { onChange = onChange
        , text = value
        , label = labelHidden "password"
        , show = False
        , placeholder = Just <| placeholder [] <| text "Password"
        }


sendButtonView : String -> Maybe msg -> Element msg
sendButtonView buttonText onPress =
    button
        [ Background.color <| Element.rgb255 251 249 255
        , Border.rounded 3
        , Border.color <| Element.rgb255 0 0 0
        , Border.width <| 1
        , width fill
        , height <| px 50
        ]
        { onPress = onPress
        , label = Element.el [ padding 10 ] <| text buttonText
        }


newPasswordInput : String -> String -> OnChange msg -> Element msg
newPasswordInput value label onChange =
    newPassword []
        { onChange = onChange
        , text = value
        , label = labelHidden "passwordConfirmation"
        , show = False
        , placeholder = Just <| placeholder [] <| Element.text label
        }


emailInput : String -> OnChange msg -> Element msg
emailInput value onChange =
    email
        []
        { text = value
        , placeholder = Just <| placeholder [] <| Element.text "Email"
        , label = labelHidden "email"
        , onChange = onChange
        }


linkView : String -> String -> Element msg
linkView url label =
    link [ Font.color <| Element.rgb255 112 97 255 ] { url = url, label = Element.text label }


titleView : Element msg
titleView =
    el [ alignTop, alignLeft, padding 20 ] <| link [ Font.size 32 ] { url = "/", label = Element.text "Servant Auth" }


logoutButton : Element msg
logoutButton =
    el
        [ alignRight
        , Font.color <| Element.rgb255 89 163 255
        ]
    <|
        link
            []
            { url = "/logout", label = Element.text "Logout" }


loginButton : Element msg
loginButton =
    el
        [ alignRight
        , Font.color <| Element.rgb255 89 163 255
        , Border.width 1
        , Border.rounded 3
        , Border.color <| Element.rgb255 0 0 0
        ]
    <|
        link
            [ padding 5 ]
            { url = "/login", label = Element.text "Login" }


unprotectedHeaderView : Element msg
unprotectedHeaderView =
    row [ padding 20, alignTop, width fill ] [ titleView ]


headerView : Maybe Session -> Element msg
headerView session =
    let
        sessionControl =
            case session of
                Just _ ->
                    logoutButton

                Nothing ->
                    loginButton
    in
    row [ padding 20, alignTop, width fill ] [ titleView, sessionControl ]
