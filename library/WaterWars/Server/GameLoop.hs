module WaterWars.Server.GameLoop where

import ClassyPrelude
import WaterWars.Core.GameState
import WaterWars.Core.GameAction
import WaterWars.Server.GameNg

-- to be forked in own thread
runGameLoop :: MonadIO m => TVar ServerState -> m ()
runGameLoop serverStateStm = do
    ServerState {..} <- atomically $ do
        serverState@(ServerState {..}) <- readTVar serverStateStm
        let newState = runGameTick gameState actions
        let newServerState = serverState { gameState = newState }
        writeTVar serverStateStm newServerState
        return newServerState
    sendGameState connections gameState


allGameTicks :: [Map Player Action] -> GameState -> [GameState]
allGameTicks [] _ = []
allGameTicks (actions:rest) initialState =
    let nextState = runGameTick initialState actions
    in initialState : allGameTicks rest nextState


data ServerState = ServerState
    { connections :: Connections
    , gameMap     :: GameMap
    , gameState   :: GameState
    , actions     :: Map Player Action
    }

-- TODO: move this data-definition to connections-management
data Connections = SomeConnections

-- TODO: move this function
sendGameState :: MonadIO m => Connections -> GameState -> m ()
sendGameState = undefined
