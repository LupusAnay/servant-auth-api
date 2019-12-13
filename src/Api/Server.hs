module Api.Server where

import Api.App
import Api.Description
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.JOSE
import Data.AuthSession
import Handlers.AuthSession
import Handlers.User
import Hasql.Pool
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server

sessionsServer :: (MonadDB m, MonadError ServerError m) => JWTSettings -> CookieSettings -> ServerT SessionAPI m
sessionsServer jwts cs = createSession cs jwts :<|> getSession :<|> deleteSession cs

usersServer :: (MonadDB m, MonadError ServerError m) => ServerT UsersAPI m
usersServer = createUser :<|> getUser :<|> getUsers :<|> patchUser :<|> deleteUser

appMToHandler :: Pool -> AppM a -> Handler a
appMToHandler pool m = runReaderT (runAppM m) pool

hoistServerWithAuth ::
     HasServer api '[ CookieSettings, JWTSettings]
  => Proxy api
  -> (forall x. m x -> n x)
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth api = hoistServerWithContext api (Proxy :: Proxy '[ CookieSettings, JWTSettings])

app :: Pool -> JWK -> Application
app pool key = do
  let jwtCfg = defaultJWTSettings key
      cookieCfg = defaultCookieSettings {cookieIsSecure = Servant.Auth.Server.NotSecure, cookieSameSite = AnySite}
      cfg = cookieCfg :. jwtCfg :. EmptyContext
      server = usersServer :<|> sessionsServer jwtCfg cookieCfg
  serveWithContext api cfg $ hoistServerWithAuth api (appMToHandler pool) server

startApp :: IO ()
startApp = do
  pool <- acquire settings
  key <- readKey "secret.key"
  run 8080 $ app pool key
  where
    settings = (1, 1, "host=localhost port=5432 user=lupusanay dbname=auth password=qwerty") -- TODO