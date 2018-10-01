module WaterWars.Server.GameLoop where

import           ClassyPrelude

import           Control.Monad.Logger

import           Control.Concurrent             ( threadDelay )

import           WaterWars.Core.Game
import           WaterWars.Core.GameNg

import           WaterWars.Server.Env
import           WaterWars.Server.ConnectionMgnt

runGameLoop
    :: (MonadLogger m, MonadIO m)
    => Env
    -> TVar GameLoopState
    -> TQueue EventMessage
    -> TVar PlayerActions
    -> m ()
runGameLoop Env {..} gameLoopStateTvar broadcastChan playerActions =
    forever $ do
        (GameLoopState {..}, gameEvents) <- atomically $ do
            gameLoopState@GameLoopState {..} <- readTVar gameLoopStateTvar
            actions                          <- emptyPlayerActions playerActions
            let (events, newState) =
                    runGameTick gameRunning gameMap gameState actions
            let newgameState = gameLoopState { gameState = newState }
            writeTVar gameLoopStateTvar newgameState
            return (newgameState, events)
        -- putStrLn $ tshow gameState
        let message = EventGameLoopMessage gameState gameEvents
        atomically $ writeTQueue broadcastChan message
        liftIO $ threadDelay (round (1000000 / gameFps))

allGameTicks :: GameMap -> [Map Player Action] -> GameState -> [GameState]
allGameTicks _ [] s = [s]
allGameTicks gameMap (actions : rest) initialState =
    initialState : allGameTicks
        gameMap
        rest
        (snd $ runGameTick True gameMap initialState actions)

emptyPlayerActions :: TVar PlayerActions -> STM (Map Player Action)
emptyPlayerActions playerActions =
    getPlayerActions <$> swapTVar playerActions (PlayerActions (mapFromList []))
