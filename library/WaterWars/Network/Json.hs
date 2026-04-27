{-# OPTIONS_GHC -Wno-orphans #-}

module WaterWars.Network.Json where

import Data.Aeson
import qualified Data.Aeson.Encoding as Encoding
import Data.Aeson.Types
import qualified Data.Array as Arr
import WaterWars.Core.Game
import WaterWars.Network.Protocol
import qualified Data.ByteString.Lazy as LBS

deriving anyclass instance ToJSON Login
deriving anyclass instance ToJSON LoginResponse
deriving anyclass instance ToJSON PlayerAction
deriving anyclass instance ToJSON GameSetup
deriving anyclass instance ToJSON GameSetupResponse
deriving anyclass instance ToJSON SetupError
deriving anyclass instance ToJSON Logout
deriving anyclass instance ToJSON ClientReady
deriving anyclass instance ToJSON GameStart
deriving anyclass instance ToJSON ServerMessage
deriving anyclass instance ToJSON ClientMessage

instance ToJSON InGamePlayer
instance ToJSON VelocityVector
instance ToJSON RunDirection
instance ToJSON Location
instance ToJSON GameMap
instance ToJSON Terrain where
  toJSON (Terrain m) = blockArrayToJSON m
  toEncoding (Terrain m) = blockArrayToEncoding m
instance ToJSON TerrainDecoration where
  toJSON (TerrainDecoration m) = blockArrayToJSON m
  toEncoding (TerrainDecoration m) = blockArrayToEncoding m
instance ToJSON Decoration
instance ToJSON BlockLocation
instance ToJSON Block
instance ToJSON BlockContent
instance ToJSON Player
instance ToJSON InGamePlayers
instance ToJSON GameState
instance ToJSON Projectiles
instance ToJSON Projectile
instance ToJSON DeadPlayers
instance ToJSON DeadPlayer
instance ToJSON GameEvents
instance ToJSON GameEvent
instance ToJSON Action
instance ToJSON RunAction
instance ToJSON JumpAction
instance ToJSON ShootAction
instance ToJSON Angle

deriving anyclass instance FromJSON Login
deriving anyclass instance FromJSON LoginResponse
deriving anyclass instance FromJSON PlayerAction
deriving anyclass instance FromJSON GameSetup
deriving anyclass instance FromJSON GameSetupResponse
deriving anyclass instance FromJSON SetupError
deriving anyclass instance FromJSON Logout
deriving anyclass instance FromJSON ClientReady
deriving anyclass instance FromJSON GameStart
deriving anyclass instance FromJSON ServerMessage
deriving anyclass instance FromJSON ClientMessage

instance FromJSON InGamePlayer
instance FromJSON VelocityVector
instance FromJSON RunDirection
instance FromJSON Location
instance FromJSON GameMap
instance FromJSON Terrain where
  parseJSON v = Terrain <$> blockArrayParseJSON v
instance FromJSON TerrainDecoration where
  parseJSON v = TerrainDecoration <$> blockArrayParseJSON v
instance FromJSON Decoration
instance FromJSON BlockLocation
instance FromJSON Block
instance FromJSON BlockContent
instance FromJSON Player
instance FromJSON InGamePlayers
instance FromJSON GameState
instance FromJSON Projectiles
instance FromJSON Projectile
instance FromJSON DeadPlayers
instance FromJSON DeadPlayer
instance FromJSON GameEvents
instance FromJSON GameEvent
instance FromJSON Action
instance FromJSON RunAction
instance FromJSON JumpAction
instance FromJSON ShootAction
instance FromJSON Angle

blockArrayToEncoding :: (ToJSON a) => Arr.Array BlockLocation a -> Encoding
blockArrayToEncoding arr =
  Encoding.pairs $
    Encoding.pair "bounds" (Encoding.list Encoding.int [x0, y0, x1, y1])
      <> Encoding.pair "elems" (Encoding.list toEncoding (Arr.elems arr))
 where
  (BlockLocation (x0, y0), BlockLocation (x1, y1)) = Arr.bounds arr

blockArrayToJSON :: (ToJSON a) => Arr.Array BlockLocation a -> Value
blockArrayToJSON arr =
  object
    [ "bounds" .= [x0, y0, x1, y1]
    , "elems" .= Arr.elems arr
    ]
 where
  (BlockLocation (x0, y0), BlockLocation (x1, y1)) = Arr.bounds arr

blockArrayParseJSON :: (FromJSON a) => Value -> Parser (Arr.Array BlockLocation a)
blockArrayParseJSON = withObject "BlockArray" $ \o -> do
  [x0, y0, x1, y1] <- o .: "bounds"
  es <- o .: "elems"
  pure $ Arr.listArray (BlockLocation (x0, y0), BlockLocation (x1, y1)) es

data JSON = JSON

instance Serializable JSON ClientMessage where
    serialize = \ _ -> LBS.toStrict . encode
    deserialize = \ _ -> eitherDecode . LBS.fromStrict

instance Serializable JSON ServerMessage where
    serialize = \ _ -> LBS.toStrict . encode
    deserialize = \ _ -> eitherDecode . LBS.fromStrict
