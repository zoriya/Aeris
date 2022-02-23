{-# LANGUAGE BlockArguments #-}

module Repository.Reaction where

import App (AppM, State (State, dbPool))
import Control.Monad.Trans.Reader (ask)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity)
import Db.Pipeline (PipelineId (PipelineId), Pipeline (Pipeline))
import Db.Reaction (Reaction (Reaction, reactionOrder), ReactionId (ReactionId), getReactionsByPipelineId, insertReaction, getWorkflow, getWorkflows, getWorkflowsByUser)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import Rel8 (asc, insert, orderBy, select, limit)
import Repository.Utils (runQuery)
import Core.User (UserId(UserId))

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
