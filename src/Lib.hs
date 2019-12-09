module Lib
  ( startApp
  , app
  ) where

import App
import Control.Exception (bracket)
import Control.Monad.Reader
import Crypto.JOSE
import qualified Data.ByteString.Char8 as BS
import Data.Generics.Internal.VL.Lens
import qualified Data.Map as M
import Hasql.Pool (Pool, acquire)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth
import Servant.Auth.Server
import SessionHandlers
import User
import UserHandlers
import Control.Monad.Except (MonadError)

type UsersAPI
   = "users" :> ReqBody '[ JSON] NewUser :> Post '[ JSON] UserId :<|>
   "users" :> Capture "id" UserId :> Get '[ JSON] User :<|>
   "users" :> Auth '[ JWT, Cookie] Session :> Get '[ JSON] [User] :<|>
   "users" :> Auth '[ JWT, Cookie] Session :> Capture "id" UserId :> ReqBody '[ JSON] NewUser :> Patch '[ JSON] User :<|>
   "users" :> Auth '[ JWT, Cookie] Session :> Capture "id" UserId :> DeleteNoContent '[ JSON] NoContent

type SessionAPI
   = "sessions" :> ReqBody '[ JSON] AuthData :> Post '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Session) :<|>
   "sessions" :> Auth '[ JWT, Cookie] Session :> Get '[ JSON] Session :<|>
   "sessions" :> Auth '[ JWT, Cookie] Session :> DeleteNoContent '[ JSON] NoContent

type API = UsersAPI :<|> SessionAPI

sessionsServer :: (MonadDB m, MonadError ServerError m) => JWTSettings -> CookieSettings -> ServerT SessionAPI m
sessionsServer jwts cs = createSession cs jwts :<|> getSession :<|> deleteSession

checkAuth :: BasicAuthData -> IO (AuthResult Session)
checkAuth (BasicAuthData "lupusanay" "qwerty") = pure $ Authenticated $ Session "0" "0" 1

usersServer :: (MonadDB m, MonadError ServerError m) => ServerT UsersAPI m
usersServer = createUser :<|> getUser :<|> getUsers :<|> patchUser :<|> deleteUser

api :: Proxy API
api = Proxy

app :: Pool -> JWK -> Application
app pool key = do
  let jwtCfg = defaultJWTSettings key
      cookieCfg =
        defaultCookieSettings
          {cookiePath = Just "/", cookieIsSecure = Servant.Auth.Server.NotSecure, cookieSameSite = AnySite}
      authCfg = checkAuth
      cfg = cookieCfg :. jwtCfg :. authCfg :. EmptyContext
  let server = usersServer :<|> sessionsServer jwtCfg cookieCfg
  serveWithContext api cfg $
    hoistServerWithContext api (Proxy :: Proxy '[ CookieSettings, JWTSettings]) appMToHandler server
  where
    appMToHandler :: AppM a -> Handler a
    appMToHandler m = runReaderT (runAppM m) pool

startApp :: IO ()
startApp = do
  pool <- acquire settings
  key <- generateKey
  run 8080 $ app pool key
  where
    settings = (1, 1, "host=localhost port=5432 user=lupusanay dbname=auth password=qwerty")