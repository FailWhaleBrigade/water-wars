{-# LANGUAGE TypeFamilies #-}
module WaterWars.Network.Protocol where

import ClassyPrelude

import WaterWars.Network.Connection
import qualified WaterWars.Core.Game.State as CoreState
import qualified WaterWars.Core.Game.Map as CoreState
import qualified WaterWars.Core.Game.Action as CoreAction

-- |Datatype to login to a game server.
-- So far, only a reconnect options is supported.
newtype Login = Login
    { sessionId :: Maybe Text
    } deriving (Show, Read, Eq)

-- |Response to a Login request.
-- Either fails with an error message or succeeds with the session id
data LoginResponse = LoginResponse
    { successSessionId :: Text
    , successPlayer    :: CoreState.InGamePlayer
    } deriving (Show, Read, Eq)

-- |Player action that can be sent to a server.
newtype PlayerAction = PlayerAction
    { getAction :: CoreAction.Action
    }
    deriving (Show, Read, Eq)

-- |Sets up a game for a single round.
data GameSetup = GameSetup
    { numberOfPlayers :: Int
    , terrainMap :: Text
    } deriving (Show, Read, Eq, Ord)

-- |Response record for a GameSetup request.
-- May fail if the game has already been set up, or the GameSetup request was invalid.
newtype GameSetupResponse = GameSetupResponse
    { getSetupResponse :: Either SetupError Bool
    } deriving (Show, Read, Eq)

-- |Signals that an error has happened during game initialization
data SetupError
    = InvalidAmountOfPlayers
    | UnknownMap
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- |If players are logging out, for completeness, not neccessarily used.
data Logout = Logout deriving (Show, Read, Eq)

data ServerMessage
    = GameSetupResponseMessage GameSetupResponse
    | LoginResponseMessage LoginResponse
    | GameMapMessage CoreState.GameMap
    | GameStateMessage CoreState.GameState
    deriving (Show, Eq, Read)

instance Serializable ServerMessage where
    serialize = tshow
instance Deserializable ServerMessage where
    deserialize = readMay

data ClientMessage
    = LoginMessage Login
    | LogoutMessage Logout
    | GameSetupMessage GameSetup
    | PlayerActionMessage PlayerAction
    deriving (Show, Eq, Read)

instance Serializable ClientMessage where
    serialize = tshow
instance Deserializable ClientMessage where
    deserialize = readMay
