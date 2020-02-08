module User exposing (User, UserId, userDecoder, usersDecoder, userIdDecoder, userIdToString)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type UserId
    = UserId Int


type alias User =
    { userId : UserId, username : String, email : String }


userIdDecoder : Decoder UserId
userIdDecoder =
    Decode.map UserId int


userDecoder : Decoder User
userDecoder =
    Decode.succeed User |> required "userId" userIdDecoder |> required "username" string |> required "email" string


usersDecoder : Decoder (List User)
usersDecoder =
    list userDecoder


userIdToString : UserId -> String
userIdToString (UserId id) =
    String.fromInt id
