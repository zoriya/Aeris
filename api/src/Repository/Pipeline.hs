{-# LANGUAGE BlockArguments #-}
module Repository.Pipeline where
import Db.Pipeline (PipelineId, Pipeline (Pipeline), getPipelineById, insertPipeline)
import App (AppM)
import Data.Functor.Identity (Identity)
import Repository.Utils (runQuery)
import Rel8 (select, limit, insert)


getPipelineById' :: PipelineId -> AppM (Pipeline Identity)
getPipelineById' pId = do
    res <- runQuery (select $ limit 1 $ getPipelineById pId)
    return $ head res 


createPipeline :: Pipeline Identity -> AppM [PipelineId]
createPipeline pipeline = runQuery (insert $ insertPipeline pipeline)
