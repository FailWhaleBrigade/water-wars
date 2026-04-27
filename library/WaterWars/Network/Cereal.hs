{-# OPTIONS_GHC -Wno-orphans #-}

module WaterWars.Network.Cereal where
import Data.Serialize
import Data.Text (Text)
import Data.Text.Encoding
import WaterWars.Core.Game
import WaterWars.Network.Protocol

deriving anyclass instance Serialize Login
deriving anyclass instance Serialize LoginResponse
deriving anyclass instance Serialize PlayerAction
deriving anyclass instance Serialize GameSetup
deriving anyclass instance Serialize GameSetupResponse
deriving anyclass instance Serialize SetupError
deriving anyclass instance Serialize Logout
deriving anyclass instance Serialize ClientReady
deriving anyclass instance Serialize GameStart
deriving anyclass instance Serialize ServerMessage
deriving anyclass instance Serialize ClientMessage

instance Serialize Text where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

instance Serialize InGamePlayer
instance Serialize VelocityVector
instance Serialize RunDirection
instance Serialize Location
instance Serialize GameMap
instance Serialize Terrain
instance Serialize TerrainDecoration
instance Serialize Decoration
instance Serialize BlockLocation
instance Serialize Block
instance Serialize BlockContent
instance Serialize Player
instance Serialize InGamePlayers
instance Serialize GameState
instance Serialize Projectiles
instance Serialize Projectile
instance Serialize DeadPlayers
instance Serialize DeadPlayer
instance Serialize GameEvents
instance Serialize GameEvent
instance Serialize Action
instance Serialize RunAction
instance Serialize JumpAction
instance Serialize ShootAction
instance Serialize Angle

data Cereal = Cereal

instance Serializable Cereal ClientMessage where
  serialize = \_ -> runPut . put
  deserialize = \_ -> runGet get

instance Serializable Cereal ServerMessage where
  serialize = \_ -> runPut . put
  deserialize = \_ -> runGet get
