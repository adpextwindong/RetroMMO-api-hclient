{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RetroMMOAPI.Headers where

import Data.Text (Text)
import Servant.API (Header', Required)
import Web.HttpApiData (ToHttpApiData (..))

type RequiredHeader = Header' '[Required]

newtype UserAgent = UserAgent Text
    deriving (Eq, Show, ToHttpApiData)

userAgent :: UserAgent
userAgent = UserAgent "retrommo-api-haskell/0.0"

type UserAgentHeader = RequiredHeader "User-Agent" UserAgent
