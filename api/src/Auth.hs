module Auth where

import Servant (Handler, RemoteHost)
import Servant.Server.Experimental.Auth
import Control.Monad.IO.Class (liftIO)

import User

data AuthAPI = AuthAPI {
	login :: RemoteHost -> Handler (Maybe String)
}