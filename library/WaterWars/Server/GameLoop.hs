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
        case runGameTick gameState actions of
            Right newState -> do
                let newServerState = serverState { gameState = newState }
                writeTVar serverStateStm newServerState
                return newServerState
            Left gameError -> undefined
    sendGameState connections gameState


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
