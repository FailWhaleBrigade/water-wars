module Codec.Resource where

import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Codec.Picture

loadPngAsBmp :: FilePath -> IO (Either String Picture)
loadPngAsBmp fp = do
    Right png <- readPng fp
    withSystemTempFile fp (\file handle -> do 
        _ <- writeDynamicBitmap file png
        bmp <- loadBMP file :: IO Picture
        return (Right bmp)
        ) :: IO (Either String Picture)
