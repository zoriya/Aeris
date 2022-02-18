{-# LANGUAGE BlockArguments #-}

module Repository.Pipeline where

import App (AppM)
import Data.Functor.Identity (Identity)
import Db.Pipeline (Pipeline (Pipeline), PipelineId, getPipelineById, insertPipeline)
import Rel8 (insert, limit, select)
import Repository.Utils (runQuery)

getPipelineById' :: PipelineId -> AppM (Pipeline Identity)
getPipelineById' pId = do
    res <- runQuery (select $ limit 1 $ getPipelineById pId)
    return $ head res

createPipeline :: Pipeline Identity -> AppM [PipelineId]
createPipeline pipeline = runQuery (insert $ insertPipeline pipeline)
