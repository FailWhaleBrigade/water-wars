module WaterWars.Server.GameLoop where

import           ClassyPrelude

import           Control.Concurrent             ( threadDelay )

import           WaterWars.Core.Game
import           WaterWars.Core.GameNg

import           WaterWars.Server.ConnectionMgnt
import           WaterWars.Server.Env

runGameLoop :: MonadIO m => Env -> m ()
runGameLoop Env {..} = forever $ do
    let ServerEnv {..}  = serverEnv
    let GameEnv {..}    = gameEnv
    let NetworkEnv {..} = networkEnv
    let GameConfig {..} = gameConfig

    (GameLoopState {..}, gameEvents) <- atomically $ do
        gameLoopState@GameLoopState {..} <- readTVar gameLoopTvar
        actions                          <- emptyPlayerActions playerActionTvar
        let (events, newState) =
                runGameTick gameRunning gameMap gameState actions
        let newgameState = gameLoopState { gameState = newState }
        writeTVar gameLoopTvar newgameState
        return (newgameState, events)
    -- putStrLn $ tshow gameState
    let message = EventGameLoopMessage gameState gameEvents
    atomically $ writeTQueue eventQueue message
    liftIO $ threadDelay (round (1000000 / fps))

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
