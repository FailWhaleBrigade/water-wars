module WaterWars.Server.State
    ( SharedState(..)
    , FutureEvent(..)
    , EventMap
    , GameMaps(..)
    , nextGameMap
    , module WaterWars.Server.ConnectionMgnt
    )
where

import           ClassyPrelude
import           WaterWars.Core.Game

import           WaterWars.Server.ConnectionMgnt

data SharedState =
    SharedState
        { eventQueue :: TQueue EventMessage
        , gameLoopTvar ::  TVar GameLoopState
        , playerActionTvar ::  TVar PlayerActions
        , connectionMapTvar ::  TVar (Map Text ClientConnection)
        , playerMapTvar  ::  TVar (Map Text InGamePlayer)
        , readyPlayersTvar ::  TVar (Set Text)
        , gameMapTvar :: TVar GameMaps
        , eventMapTvar :: TVar EventMap
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
