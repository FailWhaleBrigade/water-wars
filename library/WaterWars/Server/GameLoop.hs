module WaterWars.Server.GameLoop where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import WaterWars.Server.ConnectionMgnt
import WaterWars.Server.GameNg

-- to be forked in own thread
runGameLoop :: MonadIO m => TVar ServerState -> m ()
runGameLoop serverStateStm = do
    ServerState {..} <- atomically $ do
        serverState@ServerState {..} <- readTVar serverStateStm
        let newState = runGameTick gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    broadcastGameState connections gameState


allGameTicks :: [Map Player Action] -> GameState -> [GameState]
allGameTicks [] s = [s]
allGameTicks (actions:rest) initialState =
    initialState : allGameTicks rest (runGameTick initialState actions)


data ServerState = ServerState
    { connections :: Connections
    , gameMap     :: GameMap
    , gameState   :: GameState
    , actions     :: Map Player Action
    }

