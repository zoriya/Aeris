{-# LANGUAGE BlockArguments #-}
module Repository.Reaction where
import App (State(State, dbPool), AppM)
import Db.Reaction (Reaction(Reaction, reactionOrder), ReactionId (ReactionId), insertReaction, getReactionsByPipelineId)
import Data.Functor.Identity (Identity)
import Control.Monad.Trans.Reader (ask)
import Hasql.Transaction (statement, Transaction)
import Rel8 (insert, select, orderBy, asc)
import Hasql.Statement (Statement)
import Db.Pipeline (PipelineId(PipelineId))
import Data.Functor.Contravariant ((>$<))
import Repository.Utils (runQuery)

createReaction :: Reaction Identity -> AppM [ReactionId]
createReaction reaction = runQuery (insert $ insertReaction reaction)

getReactionsByPipelineId' :: PipelineId -> AppM [Reaction Identity]
getReactionsByPipelineId' pId = runQuery (select $ orderBy (reactionOrder >$< asc) $ getReactionsByPipelineId pId)
    
