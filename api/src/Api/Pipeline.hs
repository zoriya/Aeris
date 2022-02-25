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
import Db.Pipeline (Pipeline (Pipeline, pipelineType), PipelineId (PipelineId, toInt64), getPipelineById, insertPipeline, pipelineName, pipelineParams, pipelineSchema, pipelineId)
import Db.Reaction (Reaction (Reaction, reactionOrder, reactionParams, reactionType), ReactionId (ReactionId), getReactionsByPipelineId, insertReaction)
import GHC.Generics (Generic)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import Rel8 (asc, each, insert, limit, orderBy, select)
import Repository
    ( createPipeline,
      getPipelineById',
      getPipelineByUser,
      createReaction,
      getReactionsByPipelineId' )
import Servant (Capture, Get, JSON, err401, throwError, type (:>), NoContent (NoContent))
import Servant.API (Delete, Post, Put, ReqBody, QueryParam)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT)
import Utils (mapInd, UserAuth, AuthRes)
import Core.User (UserId(UserId), User (User))
import Servant.Auth.Server (AuthResult(Authenticated))

data PipelineData = PipelineData
    { name :: Text
    , pType :: PipelineType
    , pParams :: PipelineParams
    , id :: PipelineId
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
    { get   :: mode :- "workflow" :> UserAuth :>
        Capture "id" PipelineId :> Get '[JSON] GetPipelineResponse
    , post  :: mode :- "workflow" :> UserAuth :>
        ReqBody '[JSON] PostPipelineData :> Post '[JSON] [ReactionId]
    , put   :: mode :- "workflow" :> UserAuth :>
        Capture "id" PipelineId :> Put '[JSON] (Pipeline Identity)
    , del   :: mode :- "workflow" :> UserAuth :>
        Capture "id" PipelineId :> Delete '[JSON] (Pipeline Identity)
    , all   :: mode :- "workflows" :> UserAuth :>
        QueryParam "API_KEY" String :>Get '[JSON] [GetPipelineResponse]
    }
    deriving stock (Generic)

getPipelineHandler :: AuthRes -> PipelineId -> AppM GetPipelineResponse
getPipelineHandler (Authenticated user) pipelineId = do
    pipeline <- getPipelineById' pipelineId
    reactions <- getReactionsByPipelineId' pipelineId
    let actionResult = PipelineData (pipelineName pipeline) (pipelineType pipeline) (pipelineParams pipeline) pipelineId
    let reactionsResult = fmap (\x -> ReactionData (reactionType x) (reactionParams x)) reactions
    return $ PostPipelineData actionResult reactionsResult
getPipelineHandler _ _ = throwError err401

postPipelineHandler :: AuthRes -> PostPipelineData -> AppM [ReactionId]
postPipelineHandler (Authenticated (User uid uname slug)) x = do
    actionId <- createPipeline $ Pipeline (PipelineId 1) (name p) (pType p) (pParams p) uid
    sequence $ mapInd (reactionMap actionId) r
  where
    p = action x
    r = reactions x
    reactionMap :: PipelineId -> ReactionData -> Int -> AppM ReactionId
    reactionMap actionId s i = do
        createReaction $ Reaction (ReactionId 1) (rType s) (rParams s) actionId (fromIntegral i)
postPipelineHandler _ _ = throwError err401

putPipelineHandler :: AuthRes -> PipelineId -> AppM (Pipeline Identity)
putPipelineHandler (Authenticated user) pipelineId = throwError err401
putPipelineHandler _ _ = throwError err401

delPipelineHandler :: AuthRes -> PipelineId -> AppM (Pipeline Identity)
delPipelineHandler (Authenticated user) pipelineId = throwError err401
delPipelineHandler _ _ = throwError err401

allPipelineHandler :: AuthRes -> Maybe String -> AppM [GetPipelineResponse]
allPipelineHandler usr@(Authenticated (User uid uname slug)) Nothing = do
  pipelines <- getPipelineByUser uid
  mapM (getPipelineHandler usr . pipelineId) pipelines
allPipelineHandler _ (Just key) = return []
allPipelineHandler _ _ =  throwError err401

pipelineHandler :: PipelineAPI (AsServerT AppM)
pipelineHandler =
    PipelineAPI
        { get = getPipelineHandler
        , post = postPipelineHandler
        , put = putPipelineHandler
        , del = delPipelineHandler
        , Api.Pipeline.all = allPipelineHandler
        }
