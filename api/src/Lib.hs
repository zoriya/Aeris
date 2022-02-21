{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

module Lib where

import Api (NamedAPI, server)
import App
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Hasql.Connection as Connection
import Hasql.Pool (acquire)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)

api :: Proxy NamedAPI
api = Proxy

app :: JWTSettings -> State -> Application
app jwtCfg state =
    serveWithContext api cfg $
        hoistServerWithContext
            api
            (Proxy :: Proxy '[CookieSettings, JWTSettings])
            (`runReaderT` state)
            (Api.server cs jwtCfg)
  where
    cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
    cs = defaultCookieSettings
