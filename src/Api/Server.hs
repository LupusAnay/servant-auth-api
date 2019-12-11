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
sessionsServer jwts cs = createSession cs jwts :<|> getSession :<|> deleteSession

usersServer :: (MonadDB m, MonadError ServerError m) => ServerT UsersAPI m
usersServer = createUser :<|> getUser :<|> getUsers :<|> patchUser :<|> deleteUser

checkAuth :: BasicAuthData -> IO (AuthResult AuthSession)
checkAuth (BasicAuthData "lupusanay" "qwerty") = pure $ Authenticated $ AuthSession "0" "0" 1

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