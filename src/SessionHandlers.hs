module SessionHandlers where

import Control.Monad.IO.Class (liftIO)
import Database
import Servant
import Servant.Auth.Server
import User

blankSession :: Session
blankSession = Session "0" "0" 1

createSession ::
     (MonadDB m)
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

getSession :: AuthResult Session -> Handler Session
getSession auth = throwError err501

deleteSession :: AuthResult Session -> Handler NoContent
deleteSession auth = throwError err501