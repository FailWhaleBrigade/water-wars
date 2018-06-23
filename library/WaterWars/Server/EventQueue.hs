module WaterWars.Server.EventQueue where

import ClassyPrelude
import WaterWars.Core.Game

import WaterWars.Network.Protocol

data EventMessage
    = EventClientMessage Text ClientMessage
    | EventGameLoopMessage GameState
    deriving (Show, Eq, Read)

data GameLoopState = GameLoopState
    { gameMap     :: GameMap
    , gameState   :: GameState
    , gameRunning :: Bool
    } deriving (Show, Eq)

newtype PlayerActions = PlayerActions
    { getPlayerActions :: Map Player Action
    } deriving (Show, Eq)


modifyGameState
    :: (GameState -> a -> GameState) -> GameLoopState -> a -> GameLoopState
modifyGameState f GameLoopState {..} a =
    GameLoopState {gameState = f gameState a, ..}

startGame :: GameLoopState -> GameLoopState
startGame GameLoopState {..} = GameLoopState {gameRunning = True, ..}

stopGame :: GameLoopState -> GameLoopState
stopGame GameLoopState {..} = GameLoopState {gameRunning = False, ..}
