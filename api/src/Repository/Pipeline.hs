{-# LANGUAGE BlockArguments #-}

module Repository.Pipeline where

import App (AppM)
import Data.Functor.Identity (Identity)
import Db.Pipeline (Pipeline (Pipeline), PipelineId, getPipelineById, insertPipeline, getPipelineByUserId, selectAllPipelines, triggerPipeline, errorPipeline)
import Rel8 (insert, limit, select, update, lit)
import Repository.Utils (runQuery)
import Core.User (UserId(UserId))
import Data.Int (Int64)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Time (getCurrentTime)
import Data.Text (Text)

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

triggerPipeline' :: PipelineId -> AppM Int64 
triggerPipeline' pId = do
  currTime <- liftIO getCurrentTime
  runQuery $ update $ triggerPipeline pId currTime

errorPipeline' :: PipelineId -> Text -> AppM Int64
errorPipeline' pId msg = runQuery $ update $ errorPipeline pId msg