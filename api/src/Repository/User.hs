module Repository.User where

import App (AppM)
import Core.User (ExternalToken (service, providerId), UserId, Service)
import Data.Text (Text)
import Db.User (User', getUserByName, getUserTokensById, insertUser, selectAllUser, updateUserTokens, getUserById, updateDelTokens, UserDB (externalTokens))
import Rel8 (insert, select, update, limit, lit)
import Repository.Utils (runQuery)
import Data.Int (Int64)
import Data.List (find)
import Servant (err401, throwError)

users :: AppM [User']
users = runQuery (select selectAllUser)

getUserByName' :: Text -> AppM [User']
getUserByName' name = runQuery (select $ getUserByName name)

getUserById' :: UserId -> AppM User'
getUserById' uid = do
    res <- runQuery (select $ limit 1 $ getUserById (lit uid))
    return $ head res

getUserByToken :: ExternalToken  -> AppM User'
getUserByToken t = do
    users' <- users
    case find findByToken users' of
        Nothing -> throwError err401
        Just x -> return x
    where
        findByToken :: User' -> Bool
        findByToken usr = do
            let userTokens = externalTokens usr
            case find (\tok -> service tok == service t) userTokens of
                Nothing -> False
                Just tok -> providerId tok == providerId t


createUser :: User' -> AppM User'
createUser user = do
    ids <- runQuery (insert $ insertUser user)
    getUserById' $ head ids

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
