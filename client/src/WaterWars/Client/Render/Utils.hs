{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module WaterWars.Client.Render.Utils where

import Data.Bifunctor
import WaterWars.Core.Game (Location (..))
import Data.Function
import WaterWars.Client.Render.Config (blockSize)
import Miso (FromJSVal)
import GHC.Generics (Generic)
import Data.Bifoldable (Bifoldable)

type RealLocation = RealLocation' Double Double
newtype RealLocation' w h = RealLocation (w, h)
  deriving (Show, Eq, Ord)
  deriving newtype (Bifunctor, Bifoldable, Functor)


type Size = (Double, Double)

toRealLoc :: Size -> Location -> RealLocation
{-# INLINE toRealLoc #-}
toRealLoc (w, h) loc =
  loc
    & l2rl
    & toRealLoc' (w, h)

toRealLoc' :: Size -> RealLocation -> RealLocation
{-# INLINE toRealLoc' #-}
toRealLoc' (w, h) loc =
  loc
    & bimap (* blockSize) (* blockSize)
    & bimap (+ (w / 2)) (+ (h / 2))
    & invertHeight' h

resetCanvasOrigin :: Size -> RealLocation -> RealLocation
resetCanvasOrigin (w, h) loc =
  loc
    & bimap (subtract (w/ 2)) (id)

fromRealLoc :: Size -> RealLocation -> Location
fromRealLoc (w, h) loc =
  -- TODO: double check
  loc
    & bimap (subtract (w / 2)) (subtract (h / 2))
    & invertHeight' h
    & rl2l

toDouble :: (Real a) => a -> Double
toDouble = realToFrac @_ @Double

toFloat :: (Real a) => a -> Float
toFloat = realToFrac @_ @Float

l2rl :: Location -> RealLocation
l2rl (Location (a, b)) = RealLocation (toDouble a, toDouble b)

ll2rl :: LogicalLocation -> RealLocation
ll2rl (LogicalLocation (a, b)) = RealLocation (toDouble a, toDouble b)

rl2l :: RealLocation -> Location
rl2l (RealLocation (a, b)) = Location (toFloat a, toFloat b)

invertHeight :: Double -> Double -> Double
invertHeight height y = height - y

invertHeight' :: Bifunctor p => Double -> p a Double -> p a Double
invertHeight' height = second (invertHeight height)

newtype LogicalLocation = LogicalLocation (Int, Int)
  deriving (Show, Eq, Ord, Generic)

instance FromJSVal LogicalLocation where
