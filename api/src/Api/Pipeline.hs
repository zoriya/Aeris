{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Api.Pipeline where

import App (AppM, State (State, dbPool))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ask)
import Core.Pipeline (PipelineParams, PipelineType)
import Core.Reaction (ReactionParams, ReactionType)
import Data.Aeson (FromJSON, ToJSON, defaultOptions, eitherDecode)
import Data.Aeson.TH (deriveJSON)
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Text (Text)
import Db.Pipeline (Pipeline (Pipeline, pipelineType), PipelineId (PipelineId, toInt64), getPipelineById, insertPipeline, pipelineName, pipelineParams, pipelineSchema)
import Db.Reaction (Reaction (Reaction, reactionOrder, reactionParams, reactionType), ReactionId (ReactionId), getReactionsByPipelineId, insertReaction)
import GHC.Generics (Generic)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import Rel8 (asc, each, insert, limit, orderBy, select)
import Repository
import Servant (Capture, Get, JSON, err401, throwError, type (:>), NoContent (NoContent))
import Servant.API (Delete, Post, Put, ReqBody, QueryParam)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT)
import Utils (mapInd)

data PipelineData = PipelineData
    { name :: Text
    , pType :: PipelineType
    , pParams :: PipelineParams
    }

data ReactionData = ReactionData
    { rType :: ReactionType
    , rParams :: ReactionParams
    }

data PostPipelineData = PostPipelineData
    { action :: PipelineData
    , reactions :: [ReactionData]
    }

type GetPipelineResponse = PostPipelineData

$(deriveJSON defaultOptions ''PipelineData)
$(deriveJSON defaultOptions ''ReactionData)
$(deriveJSON defaultOptions ''PostPipelineData)

data PipelineAPI mode = PipelineAPI
    { get   :: mode :- "workflow" :> Capture "id" PipelineId :> Get '[JSON] GetPipelineResponse
    , post  :: mode :- "workflow" :> ReqBody '[JSON] PostPipelineData :> Post '[JSON] [ReactionId]
    , put   :: mode :- "workflow" :> Capture "id" PipelineId :> Put '[JSON] (Pipeline Identity)
    , del   :: mode :- "workflow" :> Capture "id" PipelineId :> Delete '[JSON] (Pipeline Identity)
    , all   :: mode :- "workflows" :> QueryParam "API_KEY" String :>Get '[JSON] NoContent 
    }
    deriving stock (Generic)

getPipelineHandler :: PipelineId -> AppM GetPipelineResponse
getPipelineHandler pipelineId = do
    pipeline <- getPipelineById' pipelineId
    reactions <- getReactionsByPipelineId' pipelineId
    let actionResult = PipelineData (pipelineName pipeline) (pipelineType pipeline) (pipelineParams pipeline)
    let reactionsResult = fmap (\x -> ReactionData (reactionType x) (reactionParams x)) reactions
    return $ PostPipelineData actionResult reactionsResult

postPipelineHandler :: PostPipelineData -> AppM [ReactionId]
postPipelineHandler x = do
    actionId <- createPipeline $ Pipeline (PipelineId 1) (name p) (pType p) (pParams p)
    sequence $ mapInd (reactionMap (head actionId)) r
  where
    p = action x
    r = reactions x
    reactionMap :: PipelineId -> ReactionData -> Int -> AppM ReactionId
    reactionMap actionId s i = do
        res <- createReaction $ Reaction (ReactionId 1) (rType s) (rParams s) actionId (fromIntegral i)
        return $ head res

putPipelineHandler :: PipelineId -> AppM (Pipeline Identity)
putPipelineHandler pipelineId = throwError err401

delPipelineHandler :: PipelineId -> AppM (Pipeline Identity)
delPipelineHandler pipelineId = throwError err401

allPipelineHandler :: Maybe String -> AppM NoContent 
allPipelineHandler Nothing = do
  --pipelines <- getPipelineByUser
  return NoContent 
allPipelineHandler (Just key) = return NoContent 

pipelineHandler :: PipelineAPI (AsServerT AppM)
pipelineHandler =
    PipelineAPI
        { get = getPipelineHandler
        , post = postPipelineHandler
        , put = putPipelineHandler
        , del = delPipelineHandler
        , Api.Pipeline.all = allPipelineHandler
        }
