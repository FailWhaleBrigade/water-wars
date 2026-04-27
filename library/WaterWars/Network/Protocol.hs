{-# OPTIONS_GHC -fno-warn-orphans #-}

module WaterWars.Network.Protocol where

import Data.ByteString
import Data.Text (Text)
import GHC.Generics
import WaterWars.Core.Game

class Serializable backend c where
  serialize :: backend -> c -> ByteString
  deserialize :: backend -> ByteString -> Either String c

-- | Datatype to login to a game server.
--  So far, only a reconnect options is supported.
newtype Login = Login
  { sessionId :: Maybe Player
  }
  deriving (Show, Read, Eq, Generic)

-- | Response to a Login request.
--  Either fails with an error message or succeeds with the session id
data LoginResponse = LoginResponse
  { successSessionId :: Player
  , successPlayer :: InGamePlayer
  }
  deriving (Show, Read, Eq, Generic)

-- | Player action that can be sent to a server.
newtype PlayerAction = PlayerAction
  { getAction :: Action
  }
  deriving (Show, Read, Eq, Generic)

-- | Sets up a game for a single round.
data GameSetup = GameSetup
  { numberOfPlayers :: Int
  , terrainMap :: Text
  }
  deriving (Show, Read, Eq, Ord, Generic)

-- | Response record for a GameSetup request.
--  May fail if the game has already been set up, or the GameSetup request was invalid.
newtype GameSetupResponse = GameSetupResponse
  { getSetupResponse :: Either SetupError Bool
  }
  deriving (Show, Read, Eq, Generic)

-- | Signals that an error has happened during game initialization
data SetupError
  = InvalidAmountOfPlayers
  | UnknownMap
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

-- | If players are logging out, for completeness, not neccessarily used.
data Logout = Logout
  deriving (Show, Read, Eq, Generic)

-- | Signals the server that a client is ready
data ClientReady = ClientReady
  deriving (Show, Read, Eq, Generic)

-- | Inform client that the game is about to start at game tick n
newtype GameStart = GameStart Integer
  deriving (Show, Read, Eq, Generic)

data ServerMessage
  = GameSetupResponseMessage GameSetupResponse
  | LoginResponseMessage LoginResponse
  | GameMapMessage GameMap
  | GameStateMessage GameState GameEvents
  | GameWillStartMessage GameStart
  | GameStartMessage
  | ResetGameMessage
  | StopGameWithWinner Player
  | StopGame
  deriving (Show, Eq, Read, Generic)

data ClientMessage
  = LoginMessage Login
  | LogoutMessage Logout
  | GameSetupMessage GameSetup
  | PlayerActionMessage PlayerAction
  | ClientReadyMessage ClientReady
  deriving (Show, Eq, Read, Generic)

