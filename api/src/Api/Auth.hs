{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
import Servant.Auth.Server (CookieSettings, JWT, JWTSettings, SetCookie, ThrowAll (throwAll), acceptLogin)
import qualified Servant.Auth.Server
import Servant.Server.Generic (AsServerT)

import Api.OIDC (OauthAPI, oauth)
import App (AppM)
import Core.User (User, UserId (UserId))
import Data.Text (pack)
import Password (hashPassword'', toPassword, validatePassword')
import Repository (createUser, getUserByName')

data LoginUser = LoginUser
    { loginUsername :: String
    , loginPassword :: String
    }
    deriving (Eq, Show, Read, Generic)

data SignupUser = SignupUser
    { signupUsername :: String
    , signupPassword :: String
    }
    deriving (Eq, Show, Read, Generic)

instance ToJSON LoginUser
instance FromJSON LoginUser

instance ToJSON SignupUser
instance FromJSON SignupUser

type Protected =
    "me" :> Get '[JSON] User

protected :: Servant.Auth.Server.AuthResult User' -> ServerT Protected AppM
protected (Servant.Auth.Server.Authenticated user) = return $ toUser user
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
    if validatePassword' (toPassword $ pack p) (password usr)
        then do
            mApplyCookies <- liftIO $ acceptLogin cs jwts usr
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
    { protectedApi ::
        mode
            :- ( Servant.Auth.Server.Auth '[JWT] User'
                    :> Protected
               )
    , unprotectedApi ::
        mode
            :- Unprotected
    , oauthApi ::
        mode
            :- OauthAPI
    }
    deriving stock (Generic)

authHandler :: CookieSettings -> JWTSettings -> AuthAPI (AsServerT AppM)
authHandler cs jwts =
    AuthAPI
        { protectedApi = protected
        , unprotectedApi = unprotected cs jwts
        , oauthApi = oauth
        }
