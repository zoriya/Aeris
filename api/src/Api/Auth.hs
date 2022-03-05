{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

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
    err400,
    err403,
    throwError,
    type (:<|>) (..),
    type (:>), Capture, QueryParam
 )

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Core.OIDC ( getOauthTokens )
import Data.Aeson (FromJSON, ToJSON)
import Db.User (User', UserDB (UserDB), password, toUser)
import GHC.Generics (Generic)
import Servant.API.Generic (ToServantApi, (:-))
import Servant.Auth.Server (CookieSettings, JWT, JWTSettings, SetCookie, ThrowAll (throwAll), acceptLogin, AuthResult (Authenticated), makeJWT)
import qualified Servant.Auth.Server
import Servant.Server.Generic (AsServerT)
import Data.ByteString.Lazy.Char8 ( unpack )
import Api.OIDC (OauthAPI, oauth)
import App (AppM)
import Core.User (User, UserId (UserId), Service)
import Data.Text (pack)
import Password (hashPassword'', toPassword, validatePassword')
import Repository (createUser, getUserByName', getUserByToken)
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

newtype LoginResponse = LoginResponse
    { jwt :: String
    }
    deriving (Eq, Show, Read, Generic)


instance ToJSON LoginResponse
instance FromJSON LoginResponse

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
        :> Post '[JSON] LoginResponse
    :<|> "signup"
        :> ReqBody '[JSON] SignupUser
        :> Post '[JSON] NoContent
    :<|> Capture "service" Service :> "signin"
        :> QueryParam "code" String
        :> Post '[JSON] LoginResponse
 

loginHandler ::
    CookieSettings ->
    JWTSettings ->
    LoginUser ->
    AppM LoginResponse
loginHandler cs jwts (LoginUser username p) = do
    users' <- getUserByName' $ pack username
    let usr = head users'
    if validatePassword' (toPassword $ pack p) (Db.User.password usr)
        then do
            etoken <- liftIO $ makeJWT (toUser usr) jwts Nothing
            case etoken of
                Left e -> throwError err401
                Right v -> return $ LoginResponse $ unpack v
        else throwError err401


loginOauthHandler :: JWTSettings -> Service -> Maybe String -> AppM LoginResponse
loginOauthHandler jwts _ Nothing = throwError err400
loginOauthHandler jwts service (Just code) = do
    tokens <- liftIO $ runMaybeT $ getOauthTokens service code
    case tokens of
        Nothing -> throwError err403 
        Just t -> do
            user <- getUserByToken t
            etoken <- liftIO $ makeJWT (toUser user) jwts Nothing
            case etoken of
                Left e -> throwError err401
                Right v -> return $ LoginResponse $ unpack v



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
    :<|> loginOauthHandler jwts

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
