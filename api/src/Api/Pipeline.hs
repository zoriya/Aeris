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
import Db.Pipeline (Pipeline (Pipeline, pipelineType), pipelineSchema, getPipelineById, PipelineId (PipelineId, toInt64), insertPipeline, pipelineParams, pipelineName)
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
import Rel8 (select, each, insert, orderBy, asc, limit)
import Core.Reaction (ReactionType, ReactionParams)
import Core.Pipeline (PipelineType, PipelineParams)
import Data.Text (Text)
import Db.Reaction (Reaction (Reaction, reactionOrder, reactionParams, reactionType), ReactionId (ReactionId), insertReaction, getReactionsByPipelineId)
import Utils (mapInd)
import Rel8.Tabulate (order)
import Data.Functor.Contravariant ((>$<))

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

type GetPipelineResponse = PostPipelineData

$(deriveJSON defaultOptions ''PipelineData)
$(deriveJSON defaultOptions ''ReactionData)
$(deriveJSON defaultOptions ''PostPipelineData)

data PipelineAPI mode = PipelineAPI
    { get   :: mode :- Capture "id" Int64 :> Get '[JSON] GetPipelineResponse
    , post  :: mode :- ReqBody '[JSON] PostPipelineData :> Post '[JSON] [ReactionId]
    , put   :: mode :- Capture "id" Int64 :> Put '[JSON] (Pipeline Identity)
    , del   :: mode :- Capture "id" Int64 :> Delete '[JSON] (Pipeline Identity)
    } deriving stock Generic

runStatement :: MonadIO m => Statement () a -> AppM a
runStatement x = do
  State{dbPool = p}  <- ask
  runTransactionWithPool p $ statement () x


getPipelineById' :: PipelineId -> AppM (Pipeline Identity)
getPipelineById' pId = do
    State{dbPool = p} <- ask
    res <- runTransactionWithPool p $ statement () (select $ limit 1 $ getPipelineById pId)
    return $ head res 

getReactionsByPipelineId' :: PipelineId -> AppM [Reaction Identity]
getReactionsByPipelineId' pId = do
    State{dbPool = p} <- ask
    runTransactionWithPool p $ statement () (select $ orderBy (reactionOrder >$< asc) $ getReactionsByPipelineId pId)
    

getPipelineHandler :: Int64 -> AppM GetPipelineResponse
getPipelineHandler pipelineId = do
    pipeline <- getPipelineById' (PipelineId pipelineId)
    reactions <- getReactionsByPipelineId' (PipelineId pipelineId)
    let actionResult = PipelineData (pipelineName pipeline) (pipelineType pipeline) (pipelineParams pipeline)
    let reactionsResult = fmap (\x -> ReactionData (reactionType x) (reactionParams x)) reactions 
    return $ PostPipelineData actionResult reactionsResult

createPipeline :: Pipeline Identity -> AppM [PipelineId]
createPipeline pipeline = do
    State{dbPool = p} <- ask
    runTransactionWithPool p $ statement () (insert $ insertPipeline pipeline)

createReaction :: Reaction Identity -> AppM [ReactionId]
createReaction reaction = do
    State{dbPool = p} <- ask
    runTransactionWithPool p $ statement () (insert $ insertReaction reaction)


postPipelineHandler :: PostPipelineData -> AppM [ReactionId]
postPipelineHandler x = do
    actionId <- createPipeline $ Pipeline (PipelineId 1) (pipelineDataName p) (pipelineDataType p) (pipelineDataParams p)
    sequence $ mapInd (reactionMap (head actionId)) r
    where
        p = action x
        r = reactions x
        reactionMap :: PipelineId -> ReactionData -> Int -> AppM ReactionId
        reactionMap actionId s i = do
            res <- createReaction $ Reaction (ReactionId 1) (reactionDataType s) (reactionDataParams s) actionId (fromIntegral i)
            return $ head res

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