module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element
import Html
import Pages.Login as Login
import Pages.NotFound as NotFound
import Pages.Registration as Registration
import Pages.Users as Users
import Route exposing (Route(..))
import Session exposing (Session)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , subscriptions = \_ -> Sub.none
        , update = update
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Page
    = NotFoundPage
    | LoginPage Login.Model
    | UsersPage Users.Model
    | RegistrationPage Registration.Model


type Msg
    = LoginPageMsg Login.Msg
    | UsersPageMsg Users.Msg
    | RegistrationPageMsg Registration.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url


type alias Model =
    { route : Route, page : Page, navKey : Nav.Key, session : Maybe Session }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url, page = NotFoundPage, navKey = navKey, session = Nothing }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Login ->
                    let
                        ( loginModel, loginCmds ) =
                            Login.init
                    in
                    ( LoginPage loginModel, Cmd.map LoginPageMsg loginCmds )

                Users ->
                    let
                        ( usersModel, usersCmds ) =
                            Users.init model.session
                    in
                    ( UsersPage usersModel, Cmd.map UsersPageMsg usersCmds )

                Registration ->
                    let
                        ( registrationModel, registrationCmds ) =
                            Registration.init model.session
                    in
                    ( RegistrationPage registrationModel, Cmd.map RegistrationPageMsg registrationCmds )

                NotFound ->
                    ( NotFoundPage, Cmd.none )
    in
    ( { model | page = currentPage }, Cmd.batch [ existingCmds, mappedPageCmds ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LoginPageMsg subMsg, LoginPage loginModel ) ->
            let
                ( updatedLoginModel, updatedCmd ) =
                    Login.update model.navKey subMsg loginModel
            in
            ( { model | page = LoginPage updatedLoginModel }, Cmd.map LoginPageMsg updatedCmd )

        ( UsersPageMsg subMsg, UsersPage usersModel ) ->
            let
                ( updatedUsersModel, updatedCmd ) =
                    Users.update subMsg usersModel
            in
            ( { model | page = UsersPage updatedUsersModel }, Cmd.map UsersPageMsg updatedCmd )

        ( RegistrationPageMsg subMsg, RegistrationPage registrationModel ) ->
            let
                ( updatedRegistrationModel, updatedCmds ) =
                    Registration.update model.navKey subMsg registrationModel
            in
            ( { model | page = RegistrationPage updatedRegistrationModel }, Cmd.map RegistrationPageMsg updatedCmds )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none ) |> initCurrentPage

        ( _, _ ) ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    let
        body =
            case model.page of
                NotFoundPage ->
                    Element.layout [] <| NotFound.view model.session

                LoginPage loginModel ->
                    Html.map LoginPageMsg <| Element.layout [] <| Login.view loginModel

                RegistrationPage registrationModel ->
                    Html.map RegistrationPageMsg <| Element.layout [] <| Registration.view registrationModel

                UsersPage usersModel ->
                    Html.map UsersPageMsg <| Element.layout [] <| Users.view usersModel
    in
    { title = "Servant Auth Api", body = [ body ] }
