{-# LANGUAGE TypeApplications #-}

module WaterWars.Client.Render.Utils where

import Data.Bifunctor
import WaterWars.Core.Game (Location (..))

toDouble :: (Real a) => a -> Double
toDouble = realToFrac @_ @Double

toFloat :: (Real a) => a -> Float
toFloat = realToFrac @_ @Float

l2d :: Location -> (Double, Double)
l2d (Location (a, b)) = (toDouble a, toDouble b)

invertHeight :: Double -> Double -> Double
invertHeight height y = height - y

invertHeight' :: Double -> (Double, Double) -> (Double, Double)
invertHeight' height = second (invertHeight height)
