module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Data.LoginForm exposing (LoginForm, emptyLoginForm)
import Data.Session as Session exposing (Session(..))
import Element exposing (Element)
import Pages.Login as Login
import Pages.NotFound as NotFound
import Pages.Registration as Registration
import Pages.Users as Users
import Route exposing (Route)
import Url exposing (Url)


type Model
    = LoginPage Login.Model
    | RegistrationPage Registration.Model
    | UsersPage Users.Model
    | NotFoundPage NotFound.Model


type Msg
    = UrlChanged Url
    | LinkClicked UrlRequest
    | GotLoginMsg Login.Msg
    | GotUsersMsg Users.Msg
    | GotRegistrationMsg Registration.Msg
    | GotNotFoundMsg NotFound.Msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = \model -> { title = "Servant Auth API", body = [ Element.layout [] <| view model ] }
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ _ key =
    ( LoginPage { session = Guest key, form = emptyLoginForm, errors = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


updateSubModel createModel createMsg updateFunc model msg =
    let
        ( newSubModel, newSubMsg ) =
            updateFunc msg model
    in
    ( createModel newSubModel, Cmd.map createMsg newSubMsg )


toSession : Model -> Session
toSession page =
    case page of
        LoginPage model ->
            model.session

        RegistrationPage model ->
            model

        UsersPage model ->
            model

        NotFoundPage model ->
            model


changeRouteTo : Route -> Model -> ( Model, Cmd Msg )
changeRouteTo route model =
    let
        session =
            toSession model
    in
    case route of
        Route.Login ->
            ( LoginPage { session = session, form = emptyLoginForm, errors = [] }, Cmd.none )

        Route.Registration ->
            ( RegistrationPage session, Cmd.none )

        Route.Users ->
            ( UsersPage session, Cmd.none )

        Route.NotFound ->
            ( NotFoundPage session, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.urlToRoute url) model

        ( GotLoginMsg subMsg, LoginPage subModel ) ->
            updateSubModel LoginPage GotLoginMsg Login.update subModel subMsg

        ( GotRegistrationMsg subMsg, RegistrationPage subModel ) ->
            updateSubModel RegistrationPage GotRegistrationMsg Registration.update subModel subMsg

        ( GotUsersMsg subMsg, UsersPage subModel ) ->
            updateSubModel UsersPage GotUsersMsg Users.update subModel subMsg

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Element Msg
view model =
    case model of
        LoginPage loginModel ->
            Element.map GotLoginMsg <| Login.view loginModel

        RegistrationPage registrationModel ->
            Element.map GotRegistrationMsg <| Registration.view registrationModel

        UsersPage usersModel ->
            Element.map GotUsersMsg <| Users.view usersModel

        NotFoundPage notFoundModel ->
            Element.map GotNotFoundMsg <| NotFound.view notFoundModel
