module WaterWars.Server.GameLoop where

import ClassyPrelude

import System.Log.Logger

import Control.Concurrent

import WaterWars.Core.GameState
import WaterWars.Core.GameAction

import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameNg

import WaterWars.Network.Protocol


-- to be forked in own thread
runGameLoop :: MonadIO m => TVar ServerState -> TChan PlayerAction -> m ()
runGameLoop serverStateStm broadcastReadSide = forever $ do
    liftIO $ debugM "Server.Connection" "Exec Game Loop tick"
    ServerState {..} <- atomically $ do
        serverState@ServerState {..} <- readTVar serverStateStm
        actions <- readAllActions broadcastReadSide
        let newState       = runGameTick gameMap gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    broadcastGameState connections gameState
    liftIO $ threadDelay 1000000 -- TODO: this sleep is necessary

readAllActions :: TChan PlayerAction -> STM (Map Player Action)
readAllActions readSide = readAllActions_ (mapFromList empty)
    where
        readAllActions_ :: Map Player Action -> STM (Map Player Action)
        readAllActions_ m = do 
            maybeAction <- tryPeekTChan readSide
            case maybeAction of 
                Nothing -> return m
                Just PlayerAction { .. } -> do 
                    let m_ = insertWith (++) player action m
                    readAllActions_ m_


allGameTicks :: GameMap -> [Map Player Action] -> GameState -> [GameState]
allGameTicks _ [] s = [s]
allGameTicks gameMap (actions : rest) initialState =
    initialState : allGameTicks gameMap
                                rest
                                (runGameTick gameMap initialState actions)