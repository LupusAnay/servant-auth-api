module UserHandlers where

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Database
import           Database.PostgreSQL.Simple (Connection)
import           Servant
import           Servant.Auth.Server
import           User

createUser :: (MonadDB m) => NewUser -> m User
createUser conn user = getUser conn 1

getUsers :: (MonadDB m) => AuthResult Session -> m [User]
getUsers (Authenticated session) = do
  user <- getUser conn 1
  pure [user]
getUsers err = throwError $ err403 {errBody = LBS.fromStrict . BS.pack . show $ err}

getUser :: (MonadDB m) => UserId -> m User
getUser id = pure $ User 1 "lupusanay" "lupusanay@gmail.com" "qwerty"

patchUser :: (MonadDB m) => AuthResult Session -> UserId -> NewUser -> m User
patchUser (Authenticated session) user id = throwError err501
patchUser _ _ _                              = throwError err403

deleteUser :: (MonadDB m) => AuthResult Session -> UserId -> m NoContent
deleteUser (Authenticated session) id = throwError err501
deleteUser _ _                           = throwError err403
