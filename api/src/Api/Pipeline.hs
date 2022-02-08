{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Pipeline where

import Servant (Capture, Get, type (:>), JSON, throwError, err401)
import Servant.API.Generic ((:-))
import GHC.Generics (Generic)
import Data.Int (Int64)
import Db.Pipeline (Pipeline)
import Data.Functor.Identity (Identity)
import Servant.API (Post, Delete, Put)
import App (AppM)
import Servant.Server.Generic (AsServerT)

data PipelineAPI mode = PipelineAPI
    { get   :: mode :- Capture "id" Int64 :> Get '[JSON] (Pipeline Identity)
    , post  :: mode :- Post '[JSON] (Pipeline Identity)
    , put   :: mode :- Capture "id" Int64 :> Put '[JSON] (Pipeline Identity)
    , del   :: mode :- Capture "id" Int64 :> Delete '[JSON] (Pipeline Identity)
    } deriving stock Generic

getPipelineHandler :: Int64 -> AppM (Pipeline Identity)
getPipelineHandler pipelineId = throwError err401

postPipelineHandler :: AppM (Pipeline Identity)
postPipelineHandler = throwError err401

putPipelineHandler :: Int64 -> AppM (Pipeline Identity)
putPipelineHandler pipelineId = throwError err401

delPipelineHandler :: Int64 -> AppM (Pipeline Identity)
delPipelineHandler pipelineId = throwError err401

pipelineHandler :: PipelineAPI (AsServerT AppM)
pipelineHandler = PipelineAPI
    { get = getPipelineHandler
    , post = postPipelineHandler
    , put = putPipelineHandler
    , del = delPipelineHandler
    }