{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text, pack)
import Db.Pipeline (Pipeline (Pipeline, pipelineType, pipelineUserId, pipelineId, pipelineEnabled), PipelineId (PipelineId, toInt64), getPipelineById, insertPipeline, pipelineName, pipelineParams, pipelineSchema, pipelineId)
import Db.Reaction (Reaction (Reaction, reactionOrder, reactionParams, reactionType), ReactionId (ReactionId), getReactionsByPipelineId, insertReaction)
import GHC.Generics (Generic)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import Rel8 (asc, each, insert, limit, orderBy, select, Expr, lit)
import Repository
    ( createPipeline,
      getPipelineById',
      getPipelineByUser,
      createReaction,
      getReactionsByPipelineId', getWorkflow', getWorkflowsByUser', getWorkflows', createReactions, putWorkflow, delWorkflow )
import Servant (Capture, Get, JSON, err401, throwError, type (:>), NoContent (NoContent), err400, err403, err500)
import Servant.API (Delete, Post, Put, ReqBody, QueryParam)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT)
import Utils (mapInd, UserAuth, AuthRes)
import Core.User (UserId(UserId), User (User))
import Servant.Auth.Server (AuthResult(Authenticated))
import Network.HTTP.Simple (setRequestBodyJSON, httpJSONEither, setRequestMethod, addRequestHeader, parseRequest, httpBS, setRequestPath)
import Network.HTTP.Client.Conduit (Request(requestBody), httpNoBody)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import System.Environment.MrEnv (envAsString)
import Data.Default (def)

data PipelineData = PipelineData
    { name :: Text
    , pType :: PipelineType
    , pParams :: PipelineParams
    , id :: PipelineId
    , enabled :: Bool
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
type PutPipelineData = PostPipelineData

$(deriveJSON defaultOptions ''PipelineData)
$(deriveJSON defaultOptions ''ReactionData)
$(deriveJSON defaultOptions ''PostPipelineData)

data PipelineAPI mode = PipelineAPI
    { get   :: mode :- "workflow" :> UserAuth :>
        Capture "id" PipelineId :> Get '[JSON] GetPipelineResponse
    , post  :: mode :- "workflow" :> UserAuth :>
        ReqBody '[JSON] PostPipelineData :> Post '[JSON] [ReactionId]
    , put   :: mode :- "workflow" :> UserAuth :>
        Capture "id" PipelineId :> ReqBody '[JSON] PutPipelineData :> Put '[JSON] PutPipelineData
    , del   :: mode :- "workflow" :> UserAuth :>
        Capture "id" PipelineId :> Delete '[JSON] Int64
    , all   :: mode :- "workflows" :> UserAuth :>
        QueryParam "API_KEY" String :>Get '[JSON] [GetPipelineResponse]
    }
    deriving stock (Generic)

formatGetPipelineResponse :: Pipeline Identity -> [Reaction Identity] -> GetPipelineResponse
formatGetPipelineResponse pipeline reactions =
    PostPipelineData actionResult reactionsResult
    where
        actionResult = PipelineData (pipelineName pipeline) (pipelineType pipeline) (pipelineParams pipeline) (pipelineId pipeline) (pipelineEnabled pipeline)
        reactionsResult = fmap (\x -> ReactionData (reactionType x) (reactionParams x)) reactions
    
informWorker :: ByteString -> PipelineId -> IO()
informWorker method id = do
    url <- envAsString "WORKER_URL" "worker/"
    request <- parseRequest url
    response <- httpBS
        $ setRequestMethod method
        $ addRequestHeader "Accept" "application/json"
        $ setRequestPath (encodeUtf8 (pack $ "/worker" <> show id))
        $ request
    return ()


getPipelineHandler :: AuthRes -> PipelineId -> AppM GetPipelineResponse
getPipelineHandler (Authenticated (User uid _ _)) pipelineId = do
    (pipeline, reactions) <- getWorkflow' pipelineId
    if pipelineUserId pipeline == uid then
        return $ formatGetPipelineResponse pipeline reactions
    else
        throwError err403 
getPipelineHandler _ _ = throwError err401

reactionDatasToReactions :: [ReactionData] -> PipelineId -> [Reaction Identity]
reactionDatasToReactions datas pId = fmap (\(s, i) -> Reaction (ReactionId 1) (rType s) (rParams s) pId (fromIntegral i)) (zip datas [0 ..])

postPipelineHandler :: AuthRes -> PostPipelineData -> AppM [ReactionId]
postPipelineHandler (Authenticated (User uid _ _)) x = do
    let newPipeline = def {
          pipelineName = name p
        , pipelineType = pType p
        , pipelineParams = pParams p
        , pipelineUserId = uid }
    actionId <- createPipeline newPipeline
    liftIO $ informWorker "POST" actionId
    createReactions $ reactionDatasToReactions (reactions x) actionId
  where
    p = action x
postPipelineHandler _ _ = throwError err401

putPipelineHandler :: AuthRes -> PipelineId -> PutPipelineData -> AppM PutPipelineData
putPipelineHandler (Authenticated (User uid _ _)) pipelineId x = do
    oldPipeline <- getPipelineById' pipelineId
    if pipelineUserId oldPipeline == uid then do
        res <- putWorkflow pipelineId newPipeline r
        if res > 0 then
            return x
        else
            throwError err500
    else
        throwError err403
    where
        p = action x
        newPipeline = lit $ def {
          pipelineName = name p
        , pipelineType = pType p
        , pipelineParams = pParams p
        , pipelineUserId = uid }
        r = reactionDatasToReactions (reactions x) pipelineId
putPipelineHandler _ _ _ = throwError err401

delPipelineHandler :: AuthRes -> PipelineId -> AppM Int64 
delPipelineHandler (Authenticated (User uid _ _)) pipelineId = do
    oldPipeline <- getPipelineById' pipelineId
    if pipelineUserId oldPipeline == uid then do
        delWorkflow pipelineId
    else throwError err403
delPipelineHandler _ _ = throwError err401

allPipelineHandler :: AuthRes -> Maybe String -> AppM [GetPipelineResponse]
allPipelineHandler usr@(Authenticated (User uid _ _)) Nothing = do
    workflows <- getWorkflowsByUser' uid
    return $ fmap (uncurry formatGetPipelineResponse) workflows
allPipelineHandler _ (Just key) = do
  k <- liftIO $ envAsString "WORKER_API_KEY" ""
  if k == key then do fmap (uncurry formatGetPipelineResponse) <$> getWorkflows' 
  else throwError err403 
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
