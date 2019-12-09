module SessionHandlers where

import App
import Control.Monad.IO.Class (liftIO)
import Servant
import Servant.Auth.Server
import User
import Control.Monad.Except (MonadError)

blankSession :: Session
blankSession = Session "0" "0" 1

createSession ::
     (MonadDB m, MonadError ServerError m)
  => CookieSettings
  -> JWTSettings
  -> AuthData
  -> m (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Session)
createSession cs jwts (AuthData "lupusanay" "qwerty") = do
  mApplyCookies <- liftIO $ acceptLogin cs jwts blankSession
  case mApplyCookies of
    Nothing -> throwError err401
    Just cookies -> pure $ cookies blankSession
createSession _ _ _ = throwError err401

getSession :: (MonadDB m, MonadError ServerError m) => AuthResult Session -> m Session
getSession auth = throwError err501

deleteSession :: (MonadDB m, MonadError ServerError m) => AuthResult Session -> m NoContent
deleteSession auth = throwError err501