module Repository.User where

import App (AppM)
import Core.User (ExternalToken, UserId)
import Data.Text (Text)
import Db.User (User', getUserByName, getUserTokensById, insertUser, selectAllUser, updateUserTokens)
import Rel8 (insert, select, update)
import Repository.Utils (runQuery)

users :: AppM [User']
users = runQuery (select selectAllUser)

getUserByName' :: Text -> AppM [User']
getUserByName' name = runQuery (select $ getUserByName name)

createUser :: User' -> AppM [UserId]
createUser user = runQuery (insert $ insertUser user)

getTokensByUserId :: UserId -> AppM [ExternalToken] 
getTokensByUserId uid = do
    res <- runQuery (select $ getUserTokensById uid)
    return $ head res

updateTokens :: UserId -> ExternalToken -> AppM ()
updateTokens uid new = do
    tokens <- getTokensByUserId uid
    runQuery (update $ updateUserTokens uid tokens new)
    return ()
