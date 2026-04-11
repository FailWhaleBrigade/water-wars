module WaterWars.Server.GameLoop
    ( runGameLoop
    )
where

import           Control.Concurrent             ( threadDelay )
import           WaterWars.Core.GameNg

import           WaterWars.Server.Env
import           WaterWars.Server.Events
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Concurrent.STM

runGameLoop :: MonadIO m => TVar Env -> TQueue EventMessage -> m ()
runGameLoop tvar queue = forever $ liftIO $ do
    Env {..} <- readTVarIO tvar
    let ServerEnv {..}     = serverEnv
    let GameEnv {..}       = gameEnv
    let GameLoopState {..} = gameLoop
    -- retrieve current actions
    let actions            = getPlayerActions playerAction
    -- run game tick
    -- TODO: make sure that no game tick has been computed twice, e.g. for every game update event, no game tick is equal
    let (events, newState) =
            runGameTick (Running == serverState) gameMap gameState actions

    -- notify game loop about state update
    let message = GameLoopMessageEvent newState events
    atomically $ writeTQueue queue message
    liftIO $ threadDelay (round (1000000 / fps gameConfig))
