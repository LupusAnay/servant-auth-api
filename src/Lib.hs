module Lib
  ( startApp
  , app
  ) where

import           Data.Generics.Internal.VL.Lens
import qualified Data.Map as M
import           Network.Wai
import           Data.Pool
import           Database
import           Control.Monad.Reader
import           Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server
import           Servant.Auth
import           User
import qualified Data.ByteString.Char8 as BS
import           UserHandlers
import           SessionHandlers
import           Control.Exception (bracket)

type UsersAPI =
        "users" :> ReqBody '[ JSON] NewUser :> Post '[ JSON] User
   :<|> "users" :> Capture "id" UserId :> Get '[ JSON] User
   :<|> "users" :> Auth '[ JWT, Cookie] Session :> Get '[ JSON] [User]
   :<|> "users" :> Auth '[ JWT, Cookie] Session :> Capture "id" UserId :> ReqBody '[ JSON] NewUser :> Patch '[ JSON] User
   :<|> "users" :> Auth '[ JWT, Cookie] Session :> Capture "id" UserId :> DeleteNoContent '[ JSON] NoContent

type SessionAPI =
        "sessions" :> ReqBody '[ JSON] AuthData :> Post '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Session)
   :<|> "sessions" :> Auth '[ JWT, Cookie] Session :> Get '[ JSON] Session
   :<|> "sessions" :> Auth '[ JWT, Cookie] Session :> DeleteNoContent '[ JSON] NoContent

type API = UsersAPI :<|> SessionAPI

sessionsServer :: JWTSettings -> CookieSettings -> Server SessionAPI
sessionsServer jwts cs = createSession cs jwts :<|> getSession :<|> deleteSession

checkAuth :: Connection -> BasicAuthData -> IO (AuthResult Session)
checkAuth db (BasicAuthData "lupusanay" "qwerty") = pure $ Authenticated $ Session "0" "0" 1

usersServer :: Server UsersAPI
usersServer = createUser :<|> getUser :<|> getUsers :<|> patchUser :<|> deleteUser

api :: Proxy API
api = Proxy

app :: Pool Connection -> IO Application
app db = do
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      cookieCfg =
        defaultCookieSettings
          {cookiePath = Just "/", cookieIsSecure = Servant.Auth.Server.NotSecure, cookieSameSite = AnySite}
      authCfg = checkAuth db
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
  pure $ serveWithContext api cfg $ hoistServer api nt $ usersServer :<|> sessionsServer jwtCfg cookieCfg
    where
      nt :: H x -> Handler a
      nt m = runReaderT (runH m) db

initConnectionPool :: String -> IO (Pool Connection)
initConnectionPool connStr = createPool (connectPostgreSQL connBStr) close 2 60 10
  where
    connBStr = BS.pack connStr

startApp :: IO ()
startApp = do
  db <- initConnectionPool "postgresql://lupusanay:qwerty@localhost/auth"
  server <- app db
  run 8080 server