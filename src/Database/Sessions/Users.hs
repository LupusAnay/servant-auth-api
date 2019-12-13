module Database.Sessions.Users where

import           Data.User
import qualified Database.Statements.Users as Statements
import           Hasql.Session
import qualified Hasql.Session             as Session

createUser :: NewUser -> Session UserId
createUser user = Session.statement user Statements.createUser

getUserById :: UserId -> Session (Maybe User)
getUserById id = Session.statement id Statements.getUserById

getUserByUsername :: String -> Session (Maybe User)
getUserByUsername name = Session.statement name Statements.getUserByUsername

getAllUsers :: Session [User]
getAllUsers = Session.statement () Statements.getAllUsers

deleteUser :: UserId -> Session ()
deleteUser id = Session.statement id Statements.deleteUser

updateUser :: UserId -> NewUser -> Session ()
updateUser id user = Session.statement (id, user) Statements.updateUser
