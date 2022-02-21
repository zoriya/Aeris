module Repository.Utils where

import App (AppM, State (State, dbPool))
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)
import Hasql.Pool (Pool, UsageError (ConnectionError, SessionError))
import qualified Hasql.Pool as Pool
import Hasql.Statement (Statement)
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Transaction.Sessions as Hasql

runTransactionWithPool :: MonadIO m => Pool -> Transaction b -> m b
runTransactionWithPool pool transaction = do
    result <- liftIO $ Pool.use pool (Hasql.transaction Hasql.Serializable Hasql.Write transaction)
    case result of
        Right e -> pure e
        Left (ConnectionError e) -> error $ "Failed to connect to database, error: " ++ show e
        Left (SessionError e) -> error $ "session error" ++ show e

runQuery :: Statement () a -> AppM a
runQuery t = do
    State{dbPool = p} <- ask
    runTransactionWithPool p $ statement () t
