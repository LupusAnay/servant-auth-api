module Pages.Users exposing (Model, Msg, init, update, view)

import Element exposing (..)
import Http exposing (Body, emptyBody)
import Session exposing (Session)
import User exposing (User, usersDecoder)
import Utils exposing (RemoteData(..), WebData, fromResult)


type alias Model =
    { users : WebData (List User), session : Maybe Session, errors : List String }


type Msg
    = GotServerResponse (WebData (List User))


init : Maybe Session -> ( Model, Cmd Msg )
init session =
    ( { users = Loading, session = session, errors = [] }, fetchUsers )


view : Model -> Element Msg
view model =
    case model.users of
        Success users ->
            column
                []
                (List.map userView users)

        Failure error ->
            text <| "Error: " ++ Debug.toString error

        Loading ->
            text "Loading"

        NotAsked ->
            text "Not asked"


userView : User -> Element Msg
userView user =
    text user.username


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotServerResponse data ->
            ( { model | users = data }, Cmd.none )


fetchUsers : Cmd Msg
fetchUsers =
    Http.riskyRequest
        { method = "GET"
        , url = "http://localhost:8080/users"
        , body = emptyBody
        , expect = Http.expectJson (fromResult >> GotServerResponse) usersDecoder
        , headers = []
        , timeout = Nothing
        , tracker = Nothing
        }
