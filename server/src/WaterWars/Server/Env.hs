module WaterWars.Server.Env where

import           ClassyPrelude           hiding ( Reader )

import           WaterWars.Core.Game

import           WaterWars.Server.Events

data Env =
    Env
        { serverEnv :: ServerEnv
        , networkEnv :: NetworkEnv
        , gameEnv :: GameEnv
        , gameConfig :: GameConfig
        } deriving (Show)

newtype NetworkEnv =
    NetworkEnv
        { connectionMap ::  Map Text Connection
        } deriving (Show)

data GameEnv =
    GameEnv
        { playerMap  ::  Map Text InGamePlayer
        , readyPlayers ::  Set Text
        , playerAction ::  PlayerActions
        } deriving (Show)

data ServerEnv =
    ServerEnv
        { gameLoop :: GameLoopState
        , eventMap :: EventMap
        , serverState :: ServerState
        } deriving (Show)

data GameConfig =
    GameConfig
        { fps :: Float
        , gameMaps :: GameMaps
        } deriving (Show)

data ServerState
    = Paused
    | Running
    | Over
    | WarmUp
    deriving (Eq, Ord, Enum, Show)

type EventMap = Map Integer FutureEvent

data GameLoopState = GameLoopState
    { gameMap     :: GameMap
    , gameState   :: GameState
    } deriving (Show, Eq)

newtype PlayerActions = PlayerActions
    { getPlayerActions :: Map Player Action
    } deriving (Show, Eq)


data GameMaps =
    GameMaps
        { gameMapsList :: Seq GameMap
        , currentGameMapIndex :: Int
        } deriving (Show, Eq, Read)

data FutureEvent
    = ResetGame
    | StartGame
    deriving (Show, Read, Eq, Ord, Enum)

advanceGameMaps :: GameMaps -> GameMaps
advanceGameMaps GameMaps {..} =
    let nextGameMapIndex = (currentGameMapIndex + 1) `mod` length gameMapsList
    in  GameMaps {currentGameMapIndex = nextGameMapIndex, ..}

currentMap :: GameMaps -> GameMap
currentMap GameMaps {..} = gameMapsList `indexEx` currentGameMapIndex

modifyGameState
    :: (GameState -> a -> GameState) -> GameLoopState -> a -> GameLoopState
modifyGameState f GameLoopState {..} a =
    GameLoopState {gameState = f gameState a, ..}
