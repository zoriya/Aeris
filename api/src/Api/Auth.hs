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

data Login = Login
  { username :: String
  , password :: String
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

type Protected
  = "me" :> Get '[JSON] User'

protected :: Servant.Auth.Server.AuthResult User' -> Server Protected
protected (Servant.Auth.Server.Authenticated user)= return user
protected _ = throwAll err401

type Unprotected
  =    "login"
    :> ReqBody '[JSON] Login
    :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> "signup"
    :> ReqBody '[JSON] User'
    :> Post '[JSON] NoContent

checkCreds  :: CookieSettings
            -> JWTSettings
            -> Login
            -> Handler (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
checkCreds cs jwts (Login username password) = do
  let usr = head users
  mApplyCookies <- liftIO $ acceptLogin cs jwts usr
  case mApplyCookies of
    Nothing -> throwError err401
    Just applyCookies -> return $ applyCookies NoContent
-- checkCreds _ _ _ = throwError err401

signup  :: User'
        -> Handler NoContent
signup usr = return NoContent

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cookieSettings jwtSetting =
  checkCreds cookieSettings jwtSetting
  :<|> signup

type API' auths = (Servant.Auth.Server.Auth auths User' :> Protected)
              :<|> Unprotected

server :: CookieSettings -> JWTSettings -> Server (API' auths)
server cs jwts = protected :<|> unprotected cs jwts

data AuthAPI mode = AuthAPI
    { login :: mode :- (Servant.Auth.Server.Auth '[JWT] User' :> Protected)
    , me :: mode :- Unprotected
    } deriving stock Generic

authHandler :: CookieSettings -> JWTSettings -> AuthAPI (AsServerT Handler)
authHandler cs jwts = AuthAPI
  { login = protected
  , me = unprotected cs jwts
  }