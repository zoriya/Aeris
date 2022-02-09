{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Api.Pipeline where

import Servant (Capture, Get, type (:>), JSON, throwError, err401)
import Servant.API.Generic ((:-))
import GHC.Generics (Generic)
import Data.Int (Int64)
import Db.Pipeline (Pipeline (Pipeline), pipelineSchema, getPipelineById, PipelineId (PipelineId), insertPipeline)
import Data.Functor.Identity (Identity)
import Servant.API (Post, Delete, Put, ReqBody)
import App (AppM, State (State, dbPool))
import Servant.Server.Generic (AsServerT)
import Hasql.Statement (Statement)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson ( eitherDecode, defaultOptions, FromJSON, ToJSON )
import Data.Aeson.TH (deriveJSON)
import Api.User (runTransactionWithPool)
import Hasql.Transaction (Transaction, statement)
import Rel8 (select, each, insert)
import Core.Reaction (ReactionType, ReactionParams)
import Core.Pipeline (PipelineType, PipelineParams)
import Data.Text (Text)


data PipelineData = PipelineData
    { pipelineDataName      :: Text
    , pipelineDataType      :: PipelineType
    , pipelineDataParams    :: PipelineParams 
    }

data ReactionData = ReactionData
    { reactionDataType    :: ReactionType
    , reactionDataParams  :: ReactionParams
    }

data PostPipelineData = PostPipelineData
    { action    :: PipelineData
    , reactions :: [ReactionData]
    }

$(deriveJSON defaultOptions ''PipelineData)
$(deriveJSON defaultOptions ''ReactionData)
$(deriveJSON defaultOptions ''PostPipelineData)

data PipelineAPI mode = PipelineAPI
    { get   :: mode :- Capture "id" Int64 :> Get '[JSON] [Pipeline Identity]
    , post  :: mode :- ReqBody '[JSON] PostPipelineData :> Post '[JSON] (Pipeline Identity)
    , put   :: mode :- Capture "id" Int64 :> Put '[JSON] (Pipeline Identity)
    , del   :: mode :- Capture "id" Int64 :> Delete '[JSON] (Pipeline Identity)
    } deriving stock Generic

runStatement :: MonadIO m => Statement () a -> AppM a
runStatement x = do
  State{dbPool = p}  <- ask
  runTransactionWithPool p $ statement () x


getPipelineHandler :: Int64 -> AppM [Pipeline Identity]
-- getPipelineHandler slugId = do
--     runStatement $ select $ getPipelineById (PipelineId slugId)
getPipelineHandler _ = throwError err401  

createPipeline :: Pipeline Identity -> AppM [PipelineId]
createPipeline pipeline = do
    State{dbPool = p} <- ask
    runTransactionWithPool p $ statement () (insert $ insertPipeline pipeline)

postPipelineHandler :: PostPipelineData -> AppM (Pipeline Identity)
postPipelineHandler x = do
    actionId <- createPipeline $ Pipeline (PipelineId 1) (pipelineDataName p) (pipelineDataType p) (pipelineDataParams p)
    throwError err401
    where
        p = action x

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