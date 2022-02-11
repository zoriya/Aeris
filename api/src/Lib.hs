{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server (defaultJWTSettings, defaultCookieSettings, generateKey, JWTSettings, CookieSettings)
import qualified Hasql.Connection as Connection
import Api ( server, NamedAPI )
import App
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Hasql.Pool (acquire)


api :: Proxy NamedAPI
api = Proxy

app :: JWTSettings -> State -> Application 
app jwtCfg state = 
    serveWithContext api cfg $
        hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (flip runReaderT state) (Api.server cs jwtCfg)
    where
        cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
        cs = defaultCookieSettings
