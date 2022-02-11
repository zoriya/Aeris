module App where

import Hasql.Pool (Pool)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Servant (Handler)

data State = State
  { dbPool :: Pool
  }

type AppM = ReaderT State Handler

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s