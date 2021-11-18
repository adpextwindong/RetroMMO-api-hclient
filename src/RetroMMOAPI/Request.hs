{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module RetroMMOAPI.Request where

import Control.Exception (throw)
import Control.Monad (void)
import Data.Text (Text, unpack)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API (Get, JSON, (:>))
import Servant.Client (ClientM, runClientM, Scheme(..), mkClientEnv, BaseUrl(..))

import RetroMMOAPI.Environment (Environment, apiEndpoint)
import RetroMMOAPI.Headers (UserAgentHeader)

type RMGet a = UserAgentHeader :> Get '[JSON] a

type Runner a = ClientM a -> IO a

run :: Environment -> Runner a
run env f = do
    mgr <- newManager tlsManagerSettings
    runWithManager mgr env f

run_ :: Environment -> Runner ()
run_ = (void .) . run

runWithManager mgr env f = either throw return =<<
    runClientM f (mkClientEnv mgr (BaseUrl Https api 443 mempty))
  where
    api = unpack $ apiEndpoint env
