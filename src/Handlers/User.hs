module Handlers.User where

import Api.App
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Except (MonadError)
import Data.AuthSession (AuthSession)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Generics.Product
import Data.User
import qualified Database.Sessions.Users as Sessions
import Servant
import Servant.Auth.Server
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Control.Monad.IO.Class (liftIO, MonadIO)

hashPassword :: (MonadIO m, MonadError ServerError m) => String -> m String
hashPassword pass = do
  hash <- liftMaybe' =<< (liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy $ BS.pack pass)
  pure $ BS.unpack hash

createUser :: (MonadDB m, MonadError ServerError m) => NewUser -> m UserId
createUser user = do
  hashedUser <- user & field @"password" hashPassword
  userId <- liftEither' =<< runSession (Sessions.createUser hashedUser)
  pure userId

getUsers :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> m [User] -- TODO: Add pagination, filtration and sort
getUsers (Authenticated session) = do
  users <- liftEither' =<< runSession Sessions.getAllUsers
  pure users
getUsers err = throwError err403 {errBody = LBS.fromStrict $ BS.pack $ show err}

getUser :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> UserId -> m User
getUser (Authenticated sess) id = do
  user <- liftEither' =<< runSession (Sessions.getUser id)
  pure user
getUser err id = throwError err403 {errBody = LBS.fromStrict $ BS.pack $ show err}

patchUser :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> UserId -> NewUser -> m NoContent
patchUser (Authenticated session) id user = do
 hashedUser <- user & field @"password" hashPassword
 liftEither' =<< runSession (Sessions.updateUser id hashedUser)
 pure NoContent
patchUser err _ _ = throwError err403 {errBody = LBS.fromStrict $ BS.pack $ show err}

deleteUser :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> UserId -> m NoContent
deleteUser (Authenticated session) id = do
  liftEither' =<< runSession (Sessions.deleteUser id)
  pure NoContent
deleteUser err _ = throwError err403 {errBody = LBS.fromStrict $ BS.pack $ show err}