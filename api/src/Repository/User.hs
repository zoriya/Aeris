module Repository.User where
import Repository.Utils (runQuery)
import App (AppM)
import Db.User (User', insertUser, getUserByName, selectAllUser, getUserTokensById, updateUserTokens)
import Data.Text (Text)
import Core.User (UserId, ExternalToken)
import Rel8 (select, insert, update)


users :: AppM [User']
users = runQuery (select selectAllUser)

getUserByName' :: Text -> AppM [User']
getUserByName' name = runQuery (select $ getUserByName name)

createUser :: User' -> AppM [UserId]
createUser user = runQuery (insert $ insertUser user)

updateTokens :: UserId -> ExternalToken  -> AppM ()
updateTokens uid new = do
    a <- runQuery (select $ getUserTokensById uid)
    runQuery (update $ updateUserTokens uid (head a) new)
    return ()