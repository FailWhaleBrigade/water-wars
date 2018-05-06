module WaterWars.Server.GameLoop where

import ClassyPrelude

import System.Log.Logger

import Control.Concurrent

import WaterWars.Core.GameState
import WaterWars.Core.GameMap
import WaterWars.Core.GameAction

import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameNg

import WaterWars.Network.Protocol
import WaterWars.Network.Connection

runGameLoop :: MonadIO m => TVar ServerState -> TMVar PlayerActions -> m ()
runGameLoop serverStateStm playerActions = forever $ do
    liftIO $ debugM "Server.Connection" "Exec Game Loop tick"
    ServerState {..} <- atomically $ do
        serverState@ServerState {..} <- readTVar serverStateStm
        actions <- getPlayerActions <$> swapTMVar playerActions (PlayerActions (mapFromList [])) 
        let newState       = runGameTick gameMap gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    liftIO $ debugM "Server.Connection" "Broadcast new State"
    broadcast connections (GameStateMessage gameState)
    liftIO $ threadDelay (1000000 `div` 60) -- TODO: this sleep is necessary

allGameTicks :: GameMap -> [Map Player Action] -> GameState -> [GameState]
allGameTicks _ [] s = [s]
allGameTicks gameMap (actions : rest) initialState =
    initialState : allGameTicks gameMap
                                rest
                                (runGameTick gameMap initialState actions)
