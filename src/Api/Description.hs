module Api.Description where

import Data.AuthSession
import Data.User
import Servant
import Servant.Auth
import Servant.Auth.Server

type UsersAPI
   = "users" :> ReqBody '[ JSON] NewUser :> Post '[ JSON] UserId :<|> 
     "users" :> Auth '[ JWT, Cookie] AuthSession :> Capture "id" UserId :> Get '[ JSON] User :<|>
     "users" :> Auth '[ JWT, Cookie] AuthSession :> Get '[ JSON] [User] :<|>
     "users" :> Auth '[ JWT, Cookie] AuthSession :> Capture "id" UserId :> ReqBody '[ JSON] NewUser :> PatchNoContent '[ JSON] NoContent :<|>
     "users" :> Auth '[ JWT, Cookie] AuthSession :> Capture "id" UserId :> DeleteNoContent '[ JSON] NoContent

type SessionAPI
   = "sessions" :> ReqBody '[ JSON] AuthData :> Post '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthSession) :<|>
     "sessions" :> Auth '[ JWT, Cookie] AuthSession :> Get '[ JSON] AuthSession :<|>
     "sessions" :> Auth '[ JWT, Cookie] AuthSession :> DeleteNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type API = UsersAPI :<|> SessionAPI

api :: Proxy API
api = Proxy