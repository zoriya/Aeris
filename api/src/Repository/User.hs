module Repository.User where

import App (AppM)
import Core.User (ExternalToken, UserId, Service)
import Data.Text (Text)
import Db.User (User', getUserByName, getUserTokensById, insertUser, selectAllUser, updateUserTokens, getUserById, updateDelTokens)
import Rel8 (insert, select, update, limit, lit)
import Repository.Utils (runQuery)
import Data.Int (Int64)

users :: AppM [User']
users = runQuery (select selectAllUser)

getUserByName' :: Text -> AppM [User']
getUserByName' name = runQuery (select $ getUserByName name)

getUserById' :: UserId -> AppM User'
getUserById' uid = do
    res <- runQuery (select $ limit 1 $ getUserById (lit uid))
    return $ head res

createUser :: User' -> AppM [UserId]
createUser user = runQuery (insert $ insertUser user)

getTokensByUserId :: UserId -> AppM [ExternalToken] 
getTokensByUserId uid = do
    res <- runQuery (select $ getUserTokensById uid)
    return $ head res

updateTokens :: UserId -> ExternalToken -> AppM Int64
updateTokens uid new = do
    tokens <- getTokensByUserId uid
    runQuery (update $ updateUserTokens uid tokens new)

delTokens :: UserId -> Service -> AppM Int64
delTokens uid service = do
    tokens <- getTokensByUserId uid
    runQuery (update $ updateDelTokens uid tokens service)
