module Codec.Resource where

import ClassyPrelude
import Graphics.Gloss
import Codec.Picture

loadPngAsBmp :: FilePath -> IO (Either String Picture)
loadPngAsBmp fp = do
    pngEither <- readPng fp
    return 
        $ do 
            png <- pngEither
            bmp <- encodeDynamicBitmap png
            Right 
                (bitmapOfByteString 
                    1920 
                    1080 
                    (BitmapFormat TopToBottom PxRGBA) 
                    (toStrict bmp) 
                    True
                )
