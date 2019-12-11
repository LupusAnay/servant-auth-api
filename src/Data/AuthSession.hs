module Data.AuthSession where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.User           (UserId)
import           GHC.Generics
import           Servant.Auth.Server

data AuthSession =
  AuthSession
    { created :: String
    , expires :: String
    , userId  :: UserId
    }
  deriving (Show, Generic, FromJSON, ToJSON, ToJWT, FromJWT)
