module Data.AuthSession where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Int            (Int64)
import           Data.User           (UserId)
import           GHC.Generics
import           Servant.Auth.Server

data AuthSession =
  AuthSession
    { created :: Int64
    , expires :: Int64
    , userId  :: UserId
    }
  deriving (Show, Generic, FromJSON, ToJSON, ToJWT, FromJWT)
