module Database.Statements.Users where

import Data.Int (Int32)
import Data.Profunctor (dimap, lmap, rmap)
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

getUserById :: Statement UserId (Maybe User)
getUserById =
  dimap
    userIdEncoder
    (userDecoder <$>)
    [TH.maybeStatement| select "user_id" :: int4, "username" :: text, email :: text, password :: text from "users" where "user_id" = $1 :: int4 |]

getUserByUsername :: Statement String (Maybe User)
getUserByUsername =
  dimap
    (\name -> T.pack name)
    (userDecoder <$>)
    [TH.maybeStatement| select "user_id" :: int4, "username" :: text, email :: text, password :: text from "users" where "username" = $1 :: text |]

getAllUsers :: Statement () [User]
getAllUsers =
  rmap
    usersDecoder
    [TH.vectorStatement| select "user_id" :: int4, "username" :: text, email :: text, password :: text from "users"|]

deleteUser :: Statement UserId ()
deleteUser = lmap userIdEncoder [TH.resultlessStatement| delete from "users" where "user_id" = $1 :: int4|]

updateUser :: Statement (UserId, NewUser) ()
updateUser =
  lmap
    userWithIdEncoder
    [TH.singletonStatement| update "users" set "username" = $2 :: text, email = $3 :: text, password = $4 :: text where "user_id" = $1 :: int4 |]

type NewUserTuple = (T.Text, T.Text, T.Text)

type UserTuple = (Int32, T.Text, T.Text, T.Text)

userWithIdEncoder :: (UserId, NewUser) -> UserTuple
userWithIdEncoder (id, user) = (userIdEncoder id, name, email, pass)
  where
    (name, email, pass) = userEncoder user

usersDecoder :: V.Vector UserTuple -> [User]
usersDecoder vec = V.toList $ V.map userDecoder vec

userDecoder :: UserTuple -> User
userDecoder (id, name, email, pass) = User (fromIntegral id) (T.unpack name) (T.unpack email) (T.unpack pass)

userIdDecoder :: Int32 -> UserId
userIdDecoder id = fromIntegral id

userIdEncoder :: UserId -> Int32
userIdEncoder id = fromIntegral id

userEncoder :: NewUser -> NewUserTuple
userEncoder (NewUser nm em pw) = (T.pack nm, T.pack em, T.pack pw)