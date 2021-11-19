{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RetroMMOAPI.Unauthenticated.API where

import Data.Proxy
import Servant.API

import Servant.Client
import ServantContrib.API.FileExtension
import RetroMMOAPI.Request (RMGet, RMRequest)
import RetroMMOAPI.Types (Username (..), UserDetails)

type JsonExt a = Ext "json" a

type RegisteredUsers = "registered-users.json"
                     :> RMGet Int

type User = "users"
          :> Capture "user" (JsonExt Username)
          :> RMGet UserDetails

type API =    User
         :<|> RegisteredUsers

api :: Proxy API
api = Proxy

getUser :: JsonExt Username -> RMRequest UserDetails
registeredUsers :: RMRequest Int
getUser :<|> registeredUsers = client api
