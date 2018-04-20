module WaterWars.Network.Protocol where

import ClassyPrelude

import qualified WaterWars.Core.GameState as CoreState
import qualified WaterWars.Core.GameAction as CoreAction

-- |Information about game that is being played.
-- It can change the current map during the game or set up the game.
-- Contains all the information neccessary to render the game for exactly one tick
data GameInformation
    = Map CoreState.GameMap
    | State CoreState.GameState
    deriving (Show, Read, Eq)

-- |Datatype to login to a game server. 
-- So far, only a reconnect options is supported.
newtype Login = Login
    { sessionId :: Maybe Text
    } deriving (Show, Read, Eq)

-- |Response to a Login request.
-- Either fails with an error message or succeeds with the session id
newtype LoginResponse = LoginResponse
    { success :: Either String String
    } deriving (Show, Read, Eq)

-- |Player action that can sent to a server.
newtype PlayerAction =
    PlayerAction CoreAction.Action
    deriving (Show, Read, Eq)

-- |Sets up a game for a single round.
data GameSetup = GameSetup
    { numberOfPlayers :: Int
    , terrainMap :: Text -- TODO: currently ignored 
    } deriving (Show, Read, Eq)

-- |Response record for a GameSetup request.
-- May fail if the game has already been set up, or the GameSetup request was invalid.
newtype GameSetupResponse = GameSetupResponse 
    { setupResponse :: Either SetupError Bool 
    } deriving (Show, Read, Eq)

-- |Signals that an error has happened during game initiliazation
data SetupError 
    = InvalidAmountOfPlayers
    | UnknownMap
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
