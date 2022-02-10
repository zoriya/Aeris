module Repository.User where
import Repository.Utils (runQuery)
import App (AppM)
import Db.User (User', insertUser, getUserByName, selectAllUser)
import Data.Text (Text)
import Core.User (UserId)
import Rel8 (select, insert)


users :: AppM [User']
users = runQuery (select selectAllUser)

getUserByName' :: Text -> AppM [User']
getUserByName' name = runQuery (select $ getUserByName name)

createUser :: User' -> AppM [UserId]
createUser user = runQuery (insert $ insertUser user)