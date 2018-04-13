module Render.Main(main) where

import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Codec.Resource (loadPngAsBmp)
import Render.Update (handleKeysIO, updateIO)
import Render.State (initialState)
import Render.Resources.Block (loadBlockMap)
import Render.Display (render)
import Control.Monad.Except

window :: Display
window = InWindow "Water Wars" (800, 600) (10, 10)
-- window = Fullscreen

fps :: Int
fps = 60

backgroundColor :: Color
backgroundColor = white

main :: IO ()
main = do
    bgTexEither  <- runExceptT $ loadPngAsBmp "resources/textures/background/background.png"
    blocksEither <- runExceptT loadBlockMap
    case bgTexEither of
        Left  err   -> putStrLn $ "Could not load texture. Cause: " ++ tshow err
        Right bgTex -> case blocksEither of
            Left err ->
                putStrLn $ "Could not load texture. Cause: " ++ tshow err
            Right blocks -> playIO window
                                   backgroundColor
                                   fps
                                   (initialState bgTex blocks)
                                   render
                                   handleKeysIO
                                   updateIO


