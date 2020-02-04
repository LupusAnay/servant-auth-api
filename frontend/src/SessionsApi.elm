module SessionsApi exposing (..)

import Data.AuthSession exposing (Session)
import Data.User exposing (AuthData, UserId)
import Http exposing (Body, jsonBody)
import Json.Decode exposing (Decoder, field, int, map3, string)
import Json.Encode as Encode


type alias HttpResult a =
    Result Http.Error a


type Msg
    = GotSession (HttpResult Session)


apiUrl : String
apiUrl =
    "http://localhost:8080/"


authDataEncoder : AuthData -> Encode.Value
authDataEncoder data =
    Encode.object [ ( "username", Encode.string data.username ), ( "password", Encode.string data.password ) ]


sessionsUrl : String
sessionsUrl =
    String.concat [ apiUrl, "sessions" ]


login : Cmd Msg
login =
    Http.post { url = sessionsUrl, body = jsonBody <| authDataEncoder <| AuthData "Fucker_3" "fuck", expect = Http.expectJson GotSession sessionDecoder }


getSession : Cmd Msg
getSession =
    Http.get { url = sessionsUrl, expect = Http.expectJson GotSession sessionDecoder }


userIdDecoder : Decoder UserId
userIdDecoder =
    field "userId" int


sessionDecoder : Decoder Session
sessionDecoder =
    map3 Session (field "created" int) (field "expires" int) (field "userId" int)
