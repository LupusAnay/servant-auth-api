module Handlers.AuthSession where

import Api.App
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt
import Data.AuthSession
import Data.AppSettings
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Product
import Data.Time.Clock.System
import Data.User
import qualified Database.Sessions.Users as Sessions
import Servant
import Servant.Auth.Server

authenticate :: String -> SystemTime -> Int -> User ->  Maybe AuthSession
authenticate pass time jwtTimeToLive user = session $ validatePassword hash password
  where
    hash = BS.pack (user ^. field @"password")
    userId = user ^. field @"userId"
    password = BS.pack pass
    session True = do
      let created = systemSeconds time
      let expires = created + (fromIntegral jwtTimeToLive)
      pure (AuthSession created expires userId)
    session False = Nothing

createSession ::
     (MonadDB m, MonadError ServerError m, MonadSettings m)
  => CookieSettings
  -> JWTSettings
  -> AuthData
  -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthSession)
createSession cs jwts (AuthData username pass) = do
  currentTime <- liftIO $ getSystemTime
  jwtTimeToLive <- getSetting jwtLifeTime
  user <- liftEither' =<< runSession (Sessions.getUserByUsername username)
  session <- liftMaybe' $ (authenticate pass currentTime jwtTimeToLive) =<< user
  liftIO $ print session
  cookies <- liftMaybe' =<< (liftIO $ acceptLogin cs jwts session)
  pure $ cookies session

getSession :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> m AuthSession
getSession (Authenticated session) = pure session
getSession err = throwError err403 {errBody = LBS.fromStrict $ BS.pack $ show err}

deleteSession ::
     (MonadDB m, MonadError ServerError m)
  => CookieSettings
  -> AuthResult AuthSession
  -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
deleteSession cs (Authenticated session) = pure $ clearSession cs NoContent
deleteSession _ err = throwError err403 {errBody = LBS.fromStrict $ BS.pack $ show err}
