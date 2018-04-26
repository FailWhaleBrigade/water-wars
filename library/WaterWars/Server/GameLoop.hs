module WaterWars.Server.GameLoop where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import WaterWars.Server.GameNg

-- to be forked in own thread
runGameLoop :: MonadIO m => TVar ServerState -> m ()
runGameLoop serverStateStm = do
    ServerState {..} <- atomically $ do
        serverState@ServerState {..} <- readTVar serverStateStm
        let newState = runGameTick gameMap gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    broadcastGameState connections gameState


allGameTicks :: GameMap -> [Map Player Action] -> GameState -> [GameState]
allGameTicks _ [] s = [s]
allGameTicks gameMap (actions:rest) initialState =
    initialState : allGameTicks gameMap rest (runGameTick gameMap initialState actions)


data ServerState = ServerState
    { connections :: Connections
    , gameMap     :: GameMap
    , gameState   :: GameState
    , actions     :: Map Player Action
    }

-- TODO: move this data-definition to connections-management
data Connections = SomeConnections

-- TODO: move this function
broadcastGameState :: MonadIO m => Connections -> GameState -> m ()
broadcastGameState = undefined
