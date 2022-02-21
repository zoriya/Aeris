{-# LANGUAGE BlockArguments #-}

module Repository.Reaction where

import App (AppM, State (State, dbPool))
import Control.Monad.Trans.Reader (ask)
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity (Identity)
import Db.Pipeline (PipelineId (PipelineId))
import Db.Reaction (Reaction (Reaction, reactionOrder), ReactionId (ReactionId), getReactionsByPipelineId, insertReaction)
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import Rel8 (asc, insert, orderBy, select)
import Repository.Utils (runQuery)

createReaction :: Reaction Identity -> AppM ReactionId
createReaction reaction = do
  res <- runQuery (insert $ insertReaction reaction)
  return $ head res

getReactionsByPipelineId' :: PipelineId -> AppM [Reaction Identity]
getReactionsByPipelineId' pId = runQuery (select $ orderBy (reactionOrder >$< asc) $ getReactionsByPipelineId pId)
