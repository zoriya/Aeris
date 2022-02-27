{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api.Worker where

import App (AppM)
import Servant (Capture, Get, JSON, err401, throwError, type (:>), NoContent (NoContent), err400, err403, err500)
import Servant.API (Delete, Post, Put, ReqBody, QueryParam)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT)
import Db.Pipeline (PipelineId(PipelineId), Pipeline (pipelineUserId))
import Utils (UserAuth, uncurry3)
import Api.Pipeline (GetPipelineResponse, formatGetPipelineResponse, allPipelineHandler)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode)
import Data.Aeson.TH (deriveJSON)
import Core.User (UserId(UserId), ExternalToken (ExternalToken))
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Environment.MrEnv (envAsString)
import Repository (getUserById', getWorkflow', getWorkflows')
import Db.User (UserDB(userDBId, externalTokens))
import Data.Functor.Identity (Identity)
import Db.Reaction (Reaction)
import GHC.Generics (Generic)


data WorkerUserData = WorkerUserData
    { userId :: UserId
    , tokens :: [ExternalToken ]
    }

data GetPipelineWorkerResponse = GetPipelineWorkerResponse
    { userData :: WorkerUserData
    , res :: GetPipelineResponse
    }

$(deriveJSON defaultOptions ''WorkerUserData)
$(deriveJSON defaultOptions ''GetPipelineWorkerResponse)

data WorkerAPI mode = WorkerAPI
    { get :: mode :- "workflow" :>Capture "id" PipelineId :>
        QueryParam "API_KEY" String :> Get '[JSON] GetPipelineWorkerResponse
    , all :: mode :- "workflows" :>
        QueryParam "API_KEY" String :> Get '[JSON] [GetPipelineWorkerResponse]
    }
    deriving stock (Generic)

fmtWorkerResponse :: Pipeline Identity -> [Reaction Identity] -> UserDB Identity -> GetPipelineWorkerResponse
fmtWorkerResponse pipeline reactions user =
    GetPipelineWorkerResponse userData res
    where
        userData = WorkerUserData (userDBId user) (externalTokens user)
        res = formatGetPipelineResponse pipeline reactions

getPipelineHandlerWorker :: PipelineId -> Maybe String -> AppM GetPipelineWorkerResponse
getPipelineHandlerWorker pId (Just key) = do
    k <- liftIO $ envAsString "WORKER_API_KEY" ""
    if k == key then uncurry3 fmtWorkerResponse <$> getWorkflow' pId
    else throwError err403
getPipelineHandlerWorker _ _ = throwError err403

allPipelineHandlerWorker :: Maybe String -> AppM [GetPipelineWorkerResponse]
allPipelineHandlerWorker (Just key) = do
  k <- liftIO $ envAsString "WORKER_API_KEY" ""
  if k == key then do fmap (uncurry3 fmtWorkerResponse) <$> getWorkflows' 
  else throwError err403 
allPipelineHandlerWorker _ = throwError err401

workerHandler :: WorkerAPI (AsServerT AppM)
workerHandler =
    WorkerAPI
        { get = getPipelineHandlerWorker
        , all = allPipelineHandlerWorker
        }