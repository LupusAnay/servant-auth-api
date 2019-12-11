module Database.Statements.Users where

import Data.Profunctor (dimap, rmap)
import qualified Data.Text as T
import Data.User
import qualified Data.Vector as V
import Hasql.Statement
import qualified Hasql.TH as TH

createUser :: Statement NewUser UserId
createUser =
  dimap
    userEncoder
    userIdDecoder
    [TH.singletonStatement| insert into "users" (username, email, password) values ($1 :: text, $2 :: text, $3 :: text) returning "user_id" :: int4 |]
  where
    userEncoder (NewUser nm em pw) = (T.pack nm, T.pack em, T.pack pw)
    userIdDecoder id = fromIntegral id

getAllUsers :: Statement () [User]
getAllUsers =
  rmap
    usersDecoder
    [TH.vectorStatement| select "user_id" :: int4, "username" :: text, email :: text, password :: text from "users"|]
  where
    tupleToUser (id, name, email, pass) = User (fromIntegral id) (T.unpack name) (T.unpack email) (T.unpack pass)
    usersDecoder vec = V.toList $ V.map tupleToUser vec