module WaterWars.Server.GameLoop
    ( runGameLoop
    )
where

import           ClassyPrelude

import           Control.Concurrent             ( threadDelay )
import           WaterWars.Core.GameNg

import           WaterWars.Server.Env
import           WaterWars.Server.Events

runGameLoop :: MonadIO m => TVar Env -> TQueue EventMessage -> m ()
runGameLoop tvar queue = forever $ do

    env                     <- readTVarIO tvar
    (gameLoop_, gameEvents) <- atomically $ do
        let ServerEnv {..}                   = serverEnv env
        let GameEnv {..}                     = gameEnv env
        let gameLoopState@GameLoopState {..} = gameLoop
        let actions                          = getPlayerActions playerAction
        let (events, newState) =
                runGameTick (Running == serverState) gameMap gameState actions
        let newgameState = gameLoopState { gameState = newState }
        return (newgameState, events)

    let message = GameLoopMessageEvent (gameState gameLoop_) gameEvents
    atomically $ writeTQueue queue message
    liftIO $ threadDelay (round (1000000 / fps (gameConfig env)))
