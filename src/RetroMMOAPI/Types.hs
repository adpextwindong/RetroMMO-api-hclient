{-# LANGUAGE DuplicateRecordFields #-} --Not sure if this is too kosher
module RetroMMOAPI.Types where

import Data.Text (Text)
import Data.Time (iso8601DateFormat, parseTimeOrError, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

type Experience = Int --newtype?
type Permissions = Int --I should ask more about this
type Rank = Int
type UserName = Text

data LeaderboardDetails = LeaderboardDetails
    { experience :: Experience
    , permissions :: Permissions
    , username :: Text
    } deriving (Eq, Show)

data UserDetails = UserDetails
    { lifetimeExperience :: Experience
    , permissions        :: Permissions
    , rank               :: Rank
    , registeredAt       :: UTCTime -- Assumed to be UTCTime
    , timePlayed         :: Float
    , userName           :: Text
    } deriving (Eq, Show)
