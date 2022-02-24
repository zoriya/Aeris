{-# LANGUAGE BlockArguments #-}

module Repository.Reaction where

import App (AppM, State (State, dbPool))
import Control.Monad.Trans.Reader (ask)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity)
import Db.Pipeline (PipelineId (PipelineId), Pipeline (Pipeline), updatePipeline, deletePipeline)
import Db.Reaction (Reaction (Reaction, reactionOrder), ReactionId (ReactionId), getReactionsByPipelineId, insertReaction, getWorkflow, getWorkflows, getWorkflowsByUser, deleteReactionsByPipelineId)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import Rel8 (asc, insert, orderBy, select, limit, update, Expr, delete)
import Repository.Utils (runQuery)
import Core.User (UserId(UserId))
import Data.Int (Int64)

createReaction :: Reaction Identity -> AppM ReactionId
createReaction reaction = do
  res <- runQuery (insert $ insertReaction reaction)
  return $ head res

getReactionsByPipelineId' :: PipelineId -> AppM [Reaction Identity]
getReactionsByPipelineId' pId = runQuery (select $ orderBy (reactionOrder >$< asc) $ getReactionsByPipelineId pId)

getWorkflow' :: PipelineId -> AppM (Pipeline Identity, [Reaction Identity])
getWorkflow' pId = do
  res <- runQuery $ select $ limit 1 $ getWorkflow pId
  return $ head res

getWorkflows' :: AppM [(Pipeline Identity, [Reaction Identity])]
getWorkflows' = runQuery $ select getWorkflows

getWorkflowsByUser' :: UserId -> AppM [(Pipeline Identity, [Reaction Identity])]
getWorkflowsByUser' uid = runQuery $ select $ getWorkflowsByUser uid

deleteReactionsByPipelineId' :: PipelineId -> AppM Int64
deleteReactionsByPipelineId' pId = runQuery $ delete $ deleteReactionsByPipelineId pId

createReactions :: [Reaction Identity] -> AppM [ReactionId]
createReactions = mapM createReaction

putWorkflow :: PipelineId -> Pipeline Expr -> [Reaction Identity] -> AppM Int64
putWorkflow pId newPipeline newReactions = do
  row <- runQuery $ update $ updatePipeline pId newPipeline
  reactionsDeleted <- deleteReactionsByPipelineId' pId
  createReactions newReactions 
  return row

delWorkflow :: PipelineId -> AppM Int64
delWorkflow pId = do
  rowsReaction <- deleteReactionsByPipelineId' pId
  row <- runQuery $ delete $ deletePipeline pId
  return $ row + rowsReaction