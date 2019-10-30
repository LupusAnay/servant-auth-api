module UserHandlers where

import           Servant
import           Servant.Auth.Server
import           User
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Char8 as BS



createUser :: NewUser -> Handler User
createUser user = getUser 1

getUsers :: AuthResult Session -> Handler [User]
getUsers (Authenticated session) = do
  user <- getUser 1
  pure [user]
getUsers err = throwError $ err403 { errBody = LBS.fromStrict . BS.pack . show $ err }

getUser :: UserId -> Handler User
getUser id = pure $ User 1 "lupusanay" "lupusanay@gmail.com" "qwerty"

patchUser :: AuthResult Session -> UserId -> NewUser -> Handler User
patchUser (Authenticated session) user id = throwError err501
patchUser _ _ _                           = throwError err403

deleteUser :: AuthResult Session -> UserId -> Handler NoContent
deleteUser (Authenticated session) id = throwError err501
deleteUser _ _                        = throwError err403
