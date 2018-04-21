module Main where

import ClassyPrelude
import Network.WebSockets

import System.Log.Logger
import System.Log.Handler.Syslog

import WaterWars.Core.DefaultGame
import WaterWars.Core.GameState
import WaterWars.Server.Config

main :: IO ()
main = do
  -- Copy everything to syslog from here on out.
  s <- openlog "water-wars-server" [PID] USER DEBUG
  updateGlobalLogger rootLoggerName (addHandler s)
  updateGlobalLogger rootLoggerName (setLevel DEBUG)
  runServer "localhost" 1234 $ \conn -> do
    connHandle <- acceptRequest conn
    debugM networkLoggerName "Client connected"
    sendTextData connHandle $ tshow (Map defaultGameMap)
    return ()
  return ()