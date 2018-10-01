module WaterWars.Server.Env
    ( Env(..)
    , NetworkEnv(..)
    , GameEnv(..)
    , GameConfig(..)
    , ServerEnv(..)
    , FutureEvent(..)
    , EventMap
    , GameMaps(..)
    , nextGameMap
    , module WaterWars.Server.ConnectionMgnt
    )
where

import           ClassyPrelude           hiding ( Reader )

import           WaterWars.Core.Game

import           WaterWars.Server.ConnectionMgnt

data Env =
    Env
        { serverEnv :: ServerEnv
        , networkEnv :: NetworkEnv
        , gameEnv :: GameEnv
        , gameConfig :: GameConfig
        }

newtype NetworkEnv =
    NetworkEnv
        { connectionMapTvar ::  TVar (Map Text ClientConnection)
        }

data GameEnv =
    GameEnv
        { playerMapTvar  ::  TVar (Map Text InGamePlayer)
        , readyPlayersTvar ::  TVar (Set Text)
        , eventMapTvar :: TVar EventMap
        }

data ServerEnv =
    ServerEnv
        { eventQueue :: TQueue EventMessage
        , gameLoopTvar ::  TVar GameLoopState
        , playerActionTvar ::  TVar PlayerActions
        }

data GameConfig =
    GameConfig
        { fps :: Float
        , gameMapTvar :: TVar GameMaps
        }

type EventMap = Map Integer FutureEvent

data GameMaps =
    GameMaps
        { gameMaps :: Seq GameMap
        , currentGameMapIndex :: Int
        } deriving (Show, Eq, Read)

data FutureEvent
    = ResetGame
    | StartGame
    deriving (Show, Read, Eq, Ord, Enum)

nextGameMap :: TVar GameMaps -> STM GameMap
nextGameMap tvar = do
    GameMaps {..} <- readTVar tvar
    let nextGameMapIndex = (currentGameMapIndex + 1) `mod` length gameMaps
    writeTVar tvar GameMaps {currentGameMapIndex = nextGameMapIndex, ..}
    return (gameMaps `indexEx` nextGameMapIndex)
