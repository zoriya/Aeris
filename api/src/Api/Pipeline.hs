{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Api.Pipeline where

import Servant (Capture, Get, type (:>), JSON, throwError, err401)
import Servant.API.Generic ((:-))
import GHC.Generics (Generic)
import Data.Int (Int64)
import Db.Pipeline (Pipeline, pipelineSchema, getPipelineById, PipelineId (PipelineId))
import Data.Functor.Identity (Identity)
import Servant.API (Post, Delete, Put)
import App (AppM, State (State, dbPool))
import Servant.Server.Generic (AsServerT)
import Hasql.Statement (Statement)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ask)
import Api.User (runTransactionWithPool)
import Hasql.Transaction (Transaction, statement)
import Rel8 (select, each)


data PostPipelineData = PostPipelineData
    { action    :: Pipeline Identity
    , reactions :: [Reaction]
    }

data PipelineAPI mode = PipelineAPI
    { get   :: mode :- Capture "id" Int64 :> Get '[JSON] [Pipeline Identity]
    , post  :: mode :- Post '[JSON] (Pipeline Identity)
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