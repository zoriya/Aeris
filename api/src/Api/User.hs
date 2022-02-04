{-# language OverloadedStrings #-}
module Api.User where

import App
import Db.User
import Rel8 (Query, select, Expr)
import qualified Hasql.Pool as Pool
import qualified Hasql.Transaction.Sessions as Hasql
import Hasql.Pool (Pool, UsageError (ConnectionError, SessionError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Hasql.Transaction (Transaction, statement)
import Control.Exception (throwIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)


runTransactionWithPool :: MonadIO m => Pool -> Transaction b -> m b
runTransactionWithPool pool transaction = do
    result <- liftIO $ Pool.use pool (Hasql.transaction Hasql.Serializable Hasql.Write transaction)
    case result of
        Right e -> pure e
        Left (ConnectionError e) -> error $ "Failed to connect to database, error: " ++ show e
        Left (SessionError e) -> error $ "session error" ++ show e

users :: AppM [User']
users = do
  State{dbPool = p}  <- ask
  runTransactionWithPool p $ statement () (select selectAllUser)


getUserByName' :: Text -> AppM [User']
getUserByName' name = do
  State{dbPool = p}  <- ask
  runTransactionWithPool p $ statement () (select $ getUserByName name)