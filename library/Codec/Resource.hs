module Codec.Resource (loadPngAsBmp, bulkLoad) where

import ClassyPrelude
import Graphics.Gloss
import qualified Codec.Picture as Juicy

loadPngAsBmp :: FilePath -> IO (Either String Picture)
loadPngAsBmp path = do
    Right img <- Juicy.readPng path
    Right _   <- Juicy.writeDynamicBitmap (path ++ ".bmp") img
    pic       <- loadBMP (path ++ ".bmp")

    return $ Right pic


bulkLoad :: Seq FilePath -> IO (Either String (Seq Picture))
bulkLoad infos = do
    loading <- mapM loadPngAsBmp infos :: IO (Seq (Either String Picture))
    let loaded = sequenceA loading :: Either String (Seq Picture)
    return loaded
