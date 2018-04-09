module Codec.Resource where

import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Codec.Picture

loadPngAsBmp :: FilePath -> IO (Either String Picture)
loadPngAsBmp fp = do
    Right png <- readPng fp
    let Right bmp = encodeDynamicBitmap png
    return 
        $ Right 
            (bitmapOfByteString 
                1920 
                1080 
                (BitmapFormat TopToBottom PxRGBA) 
                (toStrict bmp) 
                True
            )
