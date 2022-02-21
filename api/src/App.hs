{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module App where

import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Hasql.Pool (Pool)
import Servant (Handler)

data State = State
    { dbPool :: Pool
    }

type AppM = ReaderT State Handler

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s
