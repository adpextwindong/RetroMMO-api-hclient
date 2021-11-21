{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module RetroMMOAPI.Unauthenticated.API where

import Data.Proxy
import Servant.API

import Servant.Client
import ServantContrib.API.FileExtension
import RetroMMOAPI.Request (RMGet, RMRequest)
import RetroMMOAPI.Types (Username (..), UserDetails, LeaderboardDetails)

type JsonExt a = Ext "json" a

type RegisteredUsers = "registered-users.json"
                     :> RMGet Int

type User = "users"
          :> Capture "user" (JsonExt Username)
          :> RMGet UserDetails

type Players = "players.json"
             :> RMGet [Username]

type Leaderboards = "leaderboards.json"
                  :> QueryParam' '[Optional] "page" Int
                  :> RMGet [LeaderboardDetails]

type API =    User
         :<|> RegisteredUsers
         :<|> Players
         :<|> Leaderboards

api :: Proxy API
api = Proxy

getUser :: JsonExt Username -> RMRequest UserDetails
registeredUsers :: RMRequest Int
players :: RMRequest [Username]
leaderboards :: Maybe Int -> RMRequest [LeaderboardDetails]
getUser :<|> registeredUsers :<|> players :<|> leaderboards = client api
