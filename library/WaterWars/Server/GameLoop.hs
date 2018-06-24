module WaterWars.Server.GameLoop where

import ClassyPrelude

import Control.Monad.Logger

import Control.Concurrent (threadDelay)

import WaterWars.Core.Game

import WaterWars.Server.OptParse
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameNg

runGameLoop
    :: (MonadLogger m, MonadIO m)
    => Arguments
    -> TVar GameLoopState
    -> TChan EventMessage
    -> TVar PlayerActions
    -> m ()
runGameLoop Arguments {..} gameLoopStateTvar broadcastChan playerActions = forever $ do
    GameLoopState {..} <- atomically $ do
        gameLoopState@GameLoopState {..} <- readTVar gameLoopStateTvar
        actions                          <- emptyPlayerActions playerActions
        let newState     = runGameTick gameRunning gameMap gameState actions
        let newgameState = gameLoopState { gameState = newState }
        writeTVar gameLoopStateTvar newgameState
        return newgameState
    -- putStrLn $ tshow gameState
    atomically $ writeTChan broadcastChan (EventGameLoopMessage gameState)
    liftIO $ threadDelay (round (1000000 / fps))

allGameTicks :: GameMap -> [Map Player Action] -> GameState -> [GameState]
allGameTicks _ [] s = [s]
allGameTicks gameMap (actions : rest) initialState =
    initialState : allGameTicks gameMap
                                rest
                                (runGameTick True gameMap initialState actions)

emptyPlayerActions :: TVar PlayerActions -> STM (Map Player Action)
emptyPlayerActions playerActions =
    getPlayerActions <$> swapTVar playerActions (PlayerActions (mapFromList []))
