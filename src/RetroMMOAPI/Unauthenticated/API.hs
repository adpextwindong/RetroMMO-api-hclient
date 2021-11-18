{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RetroMMOAPI.Unauthenticated.API where

import Data.Proxy
import Servant.API

import Servant.Client

import RetroMMOAPI.Request (RMGet, RMRequest)
import RetroMMOAPI.Types (Username, UserDetails)

type User = "users"
          :> Capture "user" Username
          :> RMGet UserDetails

type RegisteredUsers = "registered-users.json"
                     :> RMGet Int

type API =    User
         :<|> RegisteredUsers

api :: Proxy API
api = Proxy

getUser :: Username -> RMRequest UserDetails
registeredUsers :: RMRequest Int
getUser :<|> registeredUsers = client api
