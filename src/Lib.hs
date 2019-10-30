module Lib
  ( startApp
  , app
  ) where

import Data.Generics.Internal.VL.Lens
import qualified Data.Map as M
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.Auth
import User
import UserHandlers
import SessionHandlers

type UsersAPI
   = "users" :> ReqBody '[ JSON] NewUser :> Post '[ JSON] User
   :<|> "users" :> Capture "id" UserId :> Get '[ JSON] User
   :<|> "users" :> Auth '[ JWT, Cookie] Session :> Get '[ JSON] [User]
   :<|> "users" :> Auth '[ JWT, Cookie] Session :> Capture "id" UserId :> ReqBody '[ JSON] NewUser :> Patch '[ JSON] User
   :<|> "users" :> Auth '[ JWT, Cookie] Session :> Capture "id" UserId :> DeleteNoContent '[ JSON] NoContent

type SessionAPI
   = "sessions" :> ReqBody '[ JSON] AuthData :> Post '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Session)
   :<|> "sessions" :> Auth '[ JWT, Cookie] Session :> Get '[ JSON] Session
   :<|> "sessions" :> Auth '[ JWT, Cookie] Session :> DeleteNoContent '[ JSON] NoContent

type API = UsersAPI :<|> SessionAPI

sessionsServer :: JWTSettings -> CookieSettings -> Server SessionAPI
sessionsServer jwts cs = createSession cs jwts :<|> getSession :<|> deleteSession

type Storage = M.Map UserId User
type DB = Pool Storage
type Pool a = a

checkAuth :: DB -> BasicAuthData -> IO (AuthResult Session)
checkAuth db (BasicAuthData "lupusanay" "qwerty") = pure $ Authenticated $ Session "0" "0" 1

usersServer :: Server UsersAPI
usersServer = createUser :<|> getUser :<|> getUsers :<|> patchUser :<|> deleteUser

api :: Proxy API
api = Proxy

app :: DB -> IO Application
app db = do
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      cookieCfg = defaultCookieSettings
                            { cookiePath = Just "/"
                            , cookieIsSecure = Servant.Auth.Server.NotSecure
                            , cookieSameSite = AnySite
                            }
      authCfg = checkAuth db
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
  pure $ serveWithContext api cfg $ usersServer :<|> sessionsServer jwtCfg cookieCfg

initDB :: IO DB
initDB = pure $ M.fromList [(1, User 1 "lupusanay" "lupusanay@gmail.com" "qwerty")]

startApp :: IO ()
startApp = do 
  db <- initDB
  server <- app db
  run 8080 server