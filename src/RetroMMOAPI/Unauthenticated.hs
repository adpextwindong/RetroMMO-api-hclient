module RetroMMOAPI.Unauthenticated where

import Data.Time.Clock (UTCTime)
import Servant.Client (ClientM)

import RetroMMOAPI.Headers (userAgent)
import RetroMMOAPI.Types (Username, UserDetails)
import qualified RetroMMOAPI.Unauthenticated.API as API

--TODO
getUser :: Username -> ClientM UserDetails
getUser name = API.getUser name userAgent

registeredUsers :: ClientM Int
registeredUsers = API.registeredUsers userAgent
