module Api.Sessions exposing (HttpResult, getSession, login)

import Data.AuthData exposing (AuthData, authDataEncoder)
import Data.AuthSession exposing (sessionDecoder)
import Data.Msg exposing (Msg(..))
import Http exposing (Body, jsonBody)
import Utils.Helpers exposing (liftResult)


type alias HttpResult a =
    Result Http.Error a


apiUrl : String
apiUrl =
    "http://localhost:8080/"


sessionsUrl : String
sessionsUrl =
    String.concat [ apiUrl, "sessions" ]


login : AuthData -> Cmd Msg
login data =
    Http.post
        { url = sessionsUrl
        , body = jsonBody <| authDataEncoder data
        , expect = Http.expectJson (liftResult GotSession) sessionDecoder
        }


getSession : Cmd Msg
getSession =
    Http.get { url = sessionsUrl, expect = Http.expectJson (liftResult GotSession) sessionDecoder }
