{-# LANGUAGE BlockArguments #-}

module Repository.Pipeline where

import App (AppM)
import Data.Functor.Identity (Identity)
import Db.Pipeline (Pipeline (Pipeline), PipelineId, getPipelineById, insertPipeline, getPipelineByUserId, selectAllPipelines)
import Rel8 (insert, limit, select)
import Repository.Utils (runQuery)
import Core.User (UserId(UserId))

getPipelineById' :: PipelineId -> AppM (Pipeline Identity)
getPipelineById' pId = do
    res <- runQuery (select $ limit 1 $ getPipelineById pId)
    return $ head res

getPipelineByUser :: UserId -> AppM [Pipeline Identity]
getPipelineByUser userId = runQuery (select $ getPipelineByUserId userId)

createPipeline :: Pipeline Identity -> AppM PipelineId
createPipeline pipeline = do
  res <- runQuery (insert $ insertPipeline pipeline)
  return $ head res
