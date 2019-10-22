module User where

import           Data.Aeson                              (FromJSON, ToJSON)
import           Data.Generics.Product
import           GHC.Generics

type UserId = Int

data User =
  User
    { userId   :: UserId
    , username :: String
    , email    :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data AuthData =
  AuthData
    { username :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

data NewUser =
  NewUser
    { username :: String
    , email    :: String
    , password :: String
    }
  deriving (Show, Generic, FromJSON, ToJSON)

lupus =  User {password = "qwerty", username = "lupusanay", email = "lupusanay@gmail.com", userId = 123}

lupusName = getField @"username" lupus
