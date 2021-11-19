{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RetroMMOAPI.Unauthenticated.API where

import Data.Proxy
import Servant.API

import Servant.Client
import ServantContrib.API.FileExtension
import RetroMMOAPI.Request (RMGet, RMRequest)
import RetroMMOAPI.Types (Username (..), UserDetails)

type RegisteredUsers = "registered-users"
                     :> RMGet Int

type JsonExt a = Ext "json" a

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
