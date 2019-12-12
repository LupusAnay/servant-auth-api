module Handlers.User where

import Api.App
import Control.Lens
import Control.Lens.Operators
import Control.Monad.Except (MonadError, liftEither)
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
  hashMaybe <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (BS.pack pass)
  case hashMaybe of 
    Just hash -> pure $ BS.unpack hash
    Nothing -> throwError err500

createUser :: (MonadDB m, MonadError ServerError m) => NewUser -> m UserId
createUser user = do
  hashedUser <- user & field @"password" hashPassword
  result <- runSession (Sessions.createUser hashedUser)
  case result of
    Right userId -> pure userId
    Left error -> throwError err500 {errBody = "Something went wrong with database query"}

getUsers :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> m [User]
getUsers (Authenticated session) = do
  result <- runSession Sessions.getAllUsers
  case result of
    Right users -> pure users
    Left error -> throwError err500 {errBody = "Something went wrong with database query"}
getUsers _ = throwError err403

getUser :: (MonadDB m) => UserId -> m User
getUser id = pure $ User 1 "lupusanay" "lupusanay@gmail.com" "qwerty"

patchUser :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> UserId -> NewUser -> m User
patchUser (Authenticated session) user id = throwError err501
patchUser _ _ _ = throwError err403

deleteUser :: (MonadDB m, MonadError ServerError m) => AuthResult AuthSession -> UserId -> m NoContent
deleteUser (Authenticated session) id = throwError err501
deleteUser _ _ = throwError err403