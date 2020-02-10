module Pages.Users exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Http exposing (Body)
import Session exposing (Session)
import User exposing (User, userIdToString, usersDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult, get)
import Views exposing (headerView)


type alias Model =
    { users : WebData (List User), session : Maybe Session, errors : List String }


type Msg
    = GotServerResponse (WebData (List User))


init : Maybe Session -> ( Model, Cmd Msg )
init session =
    ( { users = Loading, session = session, errors = [] }, fetchUsers )


view : Model -> Element Msg
view model =
    column [ width fill, height fill ] [ headerView model.session, el [ centerX, centerY ] <| usersView model.users ]


usersView : WebData (List User) -> Element Msg
usersView data =
    case data of
        Success users ->
            column
                [ spacing 5 ]
                (List.map userView users)

        Failure error ->
            text <| "Error: " ++ Debug.toString error

        Loading ->
            text "Loading"

        NotAsked ->
            text "Not asked"


userView : User -> Element Msg
userView user =
    row [ spacing 10 ]
        [ text <| userIdToString user.userId, text user.username, text user.email ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotServerResponse data ->
            ( { model | users = data }, Cmd.none )


fetchUsers : Cmd Msg
fetchUsers =
    get
        { path = "users"
        , expect = Http.expectJson (fromResult >> GotServerResponse) usersDecoder
        }
