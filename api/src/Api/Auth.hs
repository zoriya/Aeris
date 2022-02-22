{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth where

import Servant (
    Get,
    HasServer (ServerT),
    Header,
    Headers,
    JSON,
    NamedRoutes,
    NoContent (..),
    Post,
    ReqBody,
    err401,
    throwError,
    type (:<|>) (..),
    type (:>),
 )

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Db.User (User', UserDB (UserDB), password, toUser)
import GHC.Generics (Generic)
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Auth.Server (CookieSettings, JWT, JWTSettings, SetCookie, ThrowAll (throwAll), acceptLogin, AuthResult (Authenticated))
import qualified Servant.Auth.Server
import Servant.Server.Generic (AsServerT)

import Api.OIDC (OauthAPI, oauth)
import App (AppM)
import Core.User (User, UserId (UserId))
import Data.Text (pack)
import Password (hashPassword'', toPassword, validatePassword')
import Repository (createUser, getUserByName')
import Utils (UserAuth, AuthRes)

data LoginUser = LoginUser
    { username :: String
    , password :: String
    }
    deriving (Eq, Show, Read, Generic)

data SignupUser = SignupUser
    { username :: String
    , password :: String
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON LoginUser
instance FromJSON LoginUser

instance ToJSON SignupUser
instance FromJSON SignupUser

type Protected =
    "me" :> Get '[JSON] User

protected :: AuthRes -> ServerT Protected AppM
protected (Authenticated user) = return user
protected _ = throwAll err401

type Unprotected =
        "login"
        :> ReqBody '[JSON] LoginUser
        :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
    :<|> "signup"
        :> ReqBody '[JSON] SignupUser
        :> Post '[JSON] NoContent

loginHandler ::
    CookieSettings ->
    JWTSettings ->
    LoginUser ->
    AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler cs jwts (LoginUser username p) = do
    users' <- getUserByName' $ pack username
    let usr = head users'
    if validatePassword' (toPassword $ pack p) (Db.User.password usr)
        then do
            mApplyCookies <- liftIO $ acceptLogin cs jwts (toUser usr)
            case mApplyCookies of
                Nothing -> throwError err401
                Just applyCookies -> return $ applyCookies NoContent
        else throwError err401

signupHandler ::
    SignupUser ->
    AppM NoContent
signupHandler (SignupUser name p) = do
    hashed <- hashPassword'' $ toPassword $ pack p
    usr <- createUser $ UserDB (UserId 1) (pack name) hashed (pack name) []
    return NoContent

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected AppM
unprotected cs jwts =
    loginHandler cs jwts
        :<|> signupHandler

data AuthAPI mode = AuthAPI
    { protectedApi :: mode :- UserAuth :> Protected
    , unprotectedApi :: mode :- Unprotected
    , oauthApi :: mode :- OauthAPI
    }
    deriving stock (Generic)

authHandler :: CookieSettings -> JWTSettings -> AuthAPI (AsServerT AppM)
authHandler cs jwts =
    AuthAPI
        { protectedApi = protected
        , unprotectedApi = unprotected cs jwts
        , oauthApi = oauth
        }
