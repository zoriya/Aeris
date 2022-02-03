{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Auth where

import Servant

import qualified Servant.Auth.Server
import Servant.Auth.Server (ThrowAll(throwAll), SetCookie, CookieSettings, JWTSettings, acceptLogin, JWT)
import Control.Monad.IO.Class (liftIO)
import Db.User ( User' )
import GHC.Generics ( Generic )
import Servant.API.Generic        ((:-), ToServantApi)
import Data.Aeson (ToJSON, FromJSON)
import Servant.Server.Generic (AsServerT)

-- TODO: remove and use the user from db and not hardcoded users
import Api.User
import App (AppM)

data LoginUser = LoginUser
  { loginUsername :: String
  , loginPassword :: String
  } deriving (Eq, Show, Read, Generic)

data SignupUser = SignupUser
  { signupUsername :: String
  , signupPassword :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON LoginUser
instance FromJSON LoginUser

instance ToJSON SignupUser
instance FromJSON SignupUser

type Protected
  = "me" :> Get '[JSON] User'

protected :: Servant.Auth.Server.AuthResult User' -> ServerT Protected AppM
protected (Servant.Auth.Server.Authenticated user) = return user
protected _ = throwAll err401

type Unprotected
  =    "login"
    :> ReqBody '[JSON] LoginUser
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> "signup"
    :> ReqBody '[JSON] SignupUser
    :> Post '[JSON] NoContent

loginHandler  :: CookieSettings
            -> JWTSettings
            -> LoginUser
            -> AppM (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler cs jwts (LoginUser username password) = do
  users' <- users
  let usr = head users'
  mApplyCookies <- liftIO $ acceptLogin cs jwts usr
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> return $ applyCookies NoContent

signupHandler  :: SignupUser
        -> AppM NoContent
signupHandler usr = return NoContent

unprotected :: CookieSettings -> JWTSettings -> ServerT Unprotected AppM
unprotected cs jwts =
        loginHandler cs jwts
  :<|>  signupHandler

data AuthAPI mode = AuthAPI
    { login :: mode :- (Servant.Auth.Server.Auth '[JWT] User' :> Protected)
    , me :: mode :- Unprotected
    } deriving stock Generic

authHandler :: CookieSettings -> JWTSettings -> AuthAPI (AsServerT AppM)
authHandler cs jwts = AuthAPI
  { login = protected
  , me = unprotected cs jwts
  }