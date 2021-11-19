{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RetroMMOAPI.Unauthenticated.API where

import Data.Proxy
import Servant.API

import Servant.Client

import RetroMMOAPI.Request (RMGet, RMRequest)
import RetroMMOAPI.Types (Username, UserDetails)

--TODO write File extension Ext combinator using axmason6 & basvandijk's ideas
type User = "users"
          :> Capture "user" Username
          :> RMGet UserDetails

type RegisteredUsers = "registered-users"
                     :> RMGet Int

type API =    User
         :<|> RegisteredUsers

api :: Proxy API
api = Proxy

getUser :: Username -> RMRequest UserDetails
registeredUsers :: RMRequest Int
getUser :<|> registeredUsers = client api
