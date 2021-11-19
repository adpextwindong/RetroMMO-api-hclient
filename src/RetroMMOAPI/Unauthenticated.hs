module RetroMMOAPI.Unauthenticated where

import Data.Time.Clock (UTCTime)
import Servant.Client (ClientM)

import RetroMMOAPI.Headers (userAgent)
import RetroMMOAPI.Types (Username, UserDetails)
import qualified RetroMMOAPI.Unauthenticated.API as API
import Data.Text (pack)
import ServantContrib.API.FileExtension (liftExt)

getUser :: Username -> ClientM UserDetails
getUser name = API.getUser (liftExt name) userAgent

registeredUsers :: ClientM Int
registeredUsers = API.registeredUsers userAgent

players :: ClientM [Username]
players = API.players userAgent
