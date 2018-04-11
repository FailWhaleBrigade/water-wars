module Codec.Resource (loadPngAsBmp, bulkLoad) where

import ClassyPrelude
import Graphics.Gloss
import qualified Codec.Picture as Juicy

loadPngAsBmp :: FilePath -> IO (Either String Picture)
loadPngAsBmp path = do
    imgEither <- Juicy.readPng path
    case imgEither of
        Left  msg -> return (Left msg)
        Right img -> do
            boolEither <- Juicy.writeDynamicBitmap (path ++ ".bmp") img
            case boolEither of
                Left msg -> return (Left msg)
                Right isWritten ->
                    if isWritten
                    then
                        Right <$> loadBMP (path ++ ".bmp")
                    else
                        Left <$> return
                            ("Could not write to File: " ++ show path)


bulkLoad :: Seq FilePath -> IO (Either String (Seq Picture))
bulkLoad infos = do
    loading <- mapM loadPngAsBmp infos :: IO (Seq (Either String Picture))
    let loaded = sequenceA loading :: Either String (Seq Picture)
    return loaded


