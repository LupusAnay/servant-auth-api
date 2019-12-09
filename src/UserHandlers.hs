module UserHandlers where

import App
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Servant
import Servant.Auth.Server
import User
import Control.Monad.Except (MonadError)
import qualified Sessions

createUser :: (MonadDB m, MonadError ServerError m) => NewUser -> m UserId
createUser user = do 
  result <- runSession (Sessions.createUser user)
  case result of
    Right userId -> pure userId
    Left error -> throwError err500 {errBody = "Something went wrong with database query"}

getUsers :: (MonadDB m, MonadError ServerError m) => AuthResult Session -> m [User]
getUsers (Authenticated session) = do
  result <- runSession Sessions.getAllUsers
  case result of 
    Right users -> pure users
    Left error -> throwError err500 {errBody = "Something went wrong with database query"}
getUsers _ = throwError err403

getUser :: (MonadDB m) => UserId -> m User
getUser id = pure $ User 1 "lupusanay" "lupusanay@gmail.com" "qwerty"

patchUser :: (MonadDB m, MonadError ServerError m) => AuthResult Session -> UserId -> NewUser -> m User
patchUser (Authenticated session) user id = throwError err501
patchUser _ _ _ = throwError err403

deleteUser :: (MonadDB m, MonadError ServerError m) => AuthResult Session -> UserId -> m NoContent
deleteUser (Authenticated session) id = throwError err501
deleteUser _ _ = throwError err403