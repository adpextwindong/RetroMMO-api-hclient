{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} --Not sure if this is too kosher
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module RetroMMOAPI.Types where

import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Aeson.Types (unwrapUnaryRecords)

import Servant.API (ToHttpApiData)
import Data.Text (Text, unpack, pack)
import Data.Time (iso8601DateFormat, parseTimeOrError, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

type Experience = Int --newtype?
type Permissions = Int --I should ask more about this
type Rank = Int

newtype Username = Username { unUsername :: Text }
    deriving (Eq, Ord, ToHttpApiData)

instance Show Username where
    show (Username t) = Data.Text.unpack t

deriveJSON defaultOptions
    { unwrapUnaryRecords = True } ''Username

data LeaderboardDetails = LeaderboardDetails
    { experience  :: Experience
    , permissions :: Permissions
    , username    :: Username
    } deriving (Eq, Show)

deriveJSON defaultOptions ''LeaderboardDetails

data UserDetails = UserDetails
    { lifetimeExperience :: Experience
    , permissions        :: Permissions
    , rank               :: Rank
    , registeredAt       :: UTCTime -- Assumed to be UTCTime
    , timePlayed         :: Float
    , username           :: Username
    } deriving (Eq, Show)

deriveJSON defaultOptions ''UserDetails
