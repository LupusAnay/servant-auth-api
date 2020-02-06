module Api.Sessions exposing (HttpResult, getSession, login)

import Data.AuthData exposing (AuthData, authDataEncoder)
import Data.Session exposing (JWTData, jwtDataDecoder)
import Http exposing (Body, Error, jsonBody)


type alias HttpResult a =
    Result Http.Error a


apiUrl : String
apiUrl =
    "http://localhost:8080/"


sessionsUrl : String
sessionsUrl =
    String.concat [ apiUrl, "sessions" ]


login : AuthData -> (Result Error JWTData -> msg) -> Cmd msg
login data onResponse =
    Http.post
        { url = sessionsUrl
        , body = jsonBody <| authDataEncoder data
        , expect = Http.expectJson onResponse jwtDataDecoder
        }


getSession : (Result Error JWTData -> msg) -> Cmd msg
getSession onResponse =
    Http.get { url = sessionsUrl, expect = Http.expectJson onResponse jwtDataDecoder }
