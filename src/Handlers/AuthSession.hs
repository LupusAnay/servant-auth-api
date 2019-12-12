module Handlers.AuthSession where

import Api.App
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Data.AuthSession
import Data.User
import Servant
import Servant.Auth.Server

blankSession :: AuthSession
blankSession = AuthSession "0" "0" 1

createSession ::
     (MonadDB m, MonadError ServerError m)
  => CookieSettings
  -> JWTSettings
  -> AuthData
  -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthSession)
createSession cs jwts (AuthData "lupusanay" "qwerty") = do
  cookies <- liftMaybe' =<< liftIO (acceptLogin cs jwts blankSession)
  pure $ cookies blankSession
createSession _ _ _ = throwError err401

getSession :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> m AuthSession
getSession auth = throwError err501

deleteSession :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> m NoContent
deleteSession auth = throwError err501