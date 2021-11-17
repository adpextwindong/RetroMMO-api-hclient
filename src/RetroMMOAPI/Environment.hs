{-# LANGUAGE OverloadedStrings #-}

module RetroMMOAPI.Environment
    ( Environment (..)
    , apiEndpoint
    ) where

import Data.Text (Text)

--TODO add a sandbox version when the API becomes more complex
data Environment = Production

apiEndpoint :: Environment -> Text
apiEndpoint Production = productionAPIEndpoint
--apiEndpoint Sandbox    = sandboxAPIEndpoint

productionAPIEndpoint :: Text
productionAPIEndpoint = "play.retro-mmo.com"
