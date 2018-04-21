module WaterWars.Server.Config where

import ClassyPrelude

-- |Logger name of anything related to the network code
networkLoggerName :: String
networkLoggerName = "Server.Connection"

-- |Name of the logger related to the game engine
engineLoggerName :: String
engineLoggerName = "Server.Engine"
