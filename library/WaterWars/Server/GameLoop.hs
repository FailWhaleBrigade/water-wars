module WaterWars.Server.GameLoop where

import ClassyPrelude

import Control.Monad.Logger

import Control.Concurrent

import WaterWars.Core.Game

import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameNg

runGameLoop
    :: (MonadLogger m, MonadIO m)
    => TVar GameLoopState
    -> TChan EventMessage
    -> TVar PlayerActions
    -> m ()
runGameLoop gameLoopStateTvar broadcastChan playerActions = forever $ do
    GameLoopState {..} <- atomically $ do
        gameLoopState@GameLoopState {..} <- readTVar gameLoopStateTvar
        actions                          <- emptyPlayerActions playerActions
        let newState     = runGameTick gameMap gameState actions
        let newgameState = gameLoopState { gameState = newState }
        writeTVar gameLoopStateTvar newgameState
        return newgameState
    atomically $ writeTChan broadcastChan (EventGameLoopMessage gameState)
    liftIO $ threadDelay (1000000 `div` 60)

allGameTicks :: GameMap -> [Map Player Action] -> GameState -> [GameState]
allGameTicks _ [] s = [s]
allGameTicks gameMap (actions : rest) initialState =
    initialState : allGameTicks gameMap
                                rest
                                (runGameTick gameMap initialState actions)

emptyPlayerActions :: TVar PlayerActions -> STM (Map Player Action)
emptyPlayerActions playerActions =
    getPlayerActions <$> swapTVar playerActions (PlayerActions (mapFromList []))
