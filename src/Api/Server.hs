module Api.Server where

import Api.App
import Api.Description
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.JOSE
import Data.Aeson
import Data.AppSettings
import Data.AuthSession
import qualified Data.ByteString.Char8 as BS
import Data.Generics.Product
import Handlers.AuthSession
import Handlers.User
import Hasql.Pool
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Control.Lens ((^.))
import Control.Exception (catch, IOException)
import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)
import Data.Time (secondsToDiffTime, getCurrentTime, UTCTime, addUTCTime, utctDay, nominalDay)

sessionsServer :: (MonadDB m, MonadError ServerError m, MonadSettings m) => JWTSettings -> CookieSettings -> ServerT SessionAPI m
sessionsServer jwts cs = createSession cs jwts :<|> getSession :<|> deleteSession cs

usersServer :: (MonadDB m, MonadError ServerError m) => ServerT UsersAPI m
usersServer = createUser :<|> getUser :<|> getUsers :<|> patchUser :<|> deleteUser

appMToHandler :: AppSettings -> Pool -> AppM a -> Handler a
appMToHandler settings pool m = runReaderT (runAppM m) Env { appSettings = settings, dbPool = pool }

hoistServerWithAuth ::
     HasServer api '[ CookieSettings, JWTSettings]
  => Proxy api
  -> (forall x. m x -> n x)
  -> ServerT api m
  -> ServerT api n
hoistServerWithAuth api = hoistServerWithContext api (Proxy :: Proxy '[ CookieSettings, JWTSettings])

app :: AppSettings -> Pool -> JWK -> Application
app settings pool key = do
  let jwtCfg = defaultJWTSettings key
      cookieCfg = defaultCookieSettings { cookieIsSecure = Servant.Auth.Server.NotSecure
                                        , cookieSameSite = AnySite
                                        , cookieMaxAge = Just $ secondsToDiffTime 3600
                                        , cookieXsrfSetting = Nothing
                                        , sessionCookieName = "authToken"
                                        }
      cfg = cookieCfg :. jwtCfg :. EmptyContext
      server = usersServer :<|> sessionsServer jwtCfg cookieCfg
  serveWithContext api cfg $ hoistServerWithAuth api (appMToHandler settings pool) server

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsOrigins = Just (map BS.pack ["http://localhost:4000", "http://localhost:8080", "http://localhost"], True)
        , corsMethods = map BS.pack ["GET", "PUT", "POST", "DELETE"]
        }

startApp :: IO ()
startApp = do
  appSettings <- readSettings
  let key = fromSecret (BS.pack (appSettings ^. field @"secretKey"))
      host = appSettings ^. field @"dbHost"
      poolSize = appSettings ^. field @"dbPoolSize"
      port = appSettings ^. field @"dbPort"
      user = appSettings ^. field @"dbUser"
      name = appSettings ^. field @"dbName"
      password = appSettings ^. field @"dbPassword"
      serverPort = appSettings ^. field @"serverPort"
  print key
  pool <- acquire (poolSize, 1, BS.pack $ mconcat ["host=", host, " port=", show port, " user=", user, " dbname=", name, " password=", password])
  run serverPort $ corsWithContentType $ app appSettings pool key

readSettings :: IO AppSettings
readSettings = do
  local <- readFromCfg "settings.json.local"
  tracked <-
    case local of
      (Right settings) -> pure (Right settings)
      (Left err) -> do print err; readFromCfg "settings.json"
  print tracked
  case tracked of
    (Right settings) -> pure settings
    (Left err) -> do print err; pure defaultAppSettings

readFromCfg :: String -> IO (Either String AppSettings)
readFromCfg path = catch (eitherDecodeFileStrict path)  (\e -> pure (Left $ show (e :: IOException)))