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


-- to be forked in own thread
runGameLoop :: MonadIO m => TVar ServerState -> TChan (Maybe PlayerAction) -> TChan (Maybe PlayerAction) -> m ()
runGameLoop serverStateStm writeSide readSide = forever $ do
    liftIO $ debugM "Server.Connection" "Exec Game Loop tick"
    ServerState {..} <- atomically $ do
        serverState@ServerState {..} <- readTVar serverStateStm
        writeTChan writeSide Nothing
        actions <- readAllActions readSide
        let newState       = runGameTick gameMap gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    liftIO $ debugM "Server.Connection" $ "Broadcast new State: " ++ show gameState
    broadcastGameState connections gameState
    liftIO $ threadDelay (1000000 `div` 60) -- TODO: this sleep is necessary

readAllActions :: TChan (Maybe PlayerAction) -> STM (Map Player Action)
readAllActions readSide = readAllActions_ (mapFromList empty)
    where
        readAllActions_ :: Map Player Action -> STM (Map Player Action)
        readAllActions_ m = do
            maybeAction <- readTChan readSide
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
