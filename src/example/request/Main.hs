{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)

import RetroMMOAPI.Types
import RetroMMOAPI.Request
import RetroMMOAPI.Environment
import RetroMMOAPI.Unauthenticated

main :: IO ()
main = do
    run Production $ do
        getUser (Username "Evan") >>= liftIO . print
        registeredUsers >>= liftIO . print
