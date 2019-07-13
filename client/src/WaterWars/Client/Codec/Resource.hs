module WaterWars.Client.Codec.Resource (loadPngAsBmp, bulkLoad) where

import ClassyPrelude
import qualified Codec.Picture as Juicy
import Control.Monad.Error.Class

import Graphics.Gloss

loadPngAsBmp :: (MonadIO m, MonadError Text m) => FilePath -> m Picture
loadPngAsBmp path = do
    imgEither <- liftIO $ Juicy.readPng path
    img       <- case imgEither of
        Left  msg -> throwError (pack msg)
        Right i   -> return i

    boolEither <- liftIO $ Juicy.writeDynamicBitmap (path ++ ".bmp") img
    isWritten  <- case boolEither of
        Left  msg -> throwError (pack msg)
        Right b   -> return b

    if not isWritten
        then throwError ("Could not write to File: " ++ tshow path)
        else liftIO (loadBMP (path ++ ".bmp"))
{--
loadPngInMemory :: (MonadIO m, MonadError String m) => FilePath -> m (Juicy.Image Juicy.PixelRGBA8)
loadPngInMemory path = do
    imgEither <- liftIO $ Juicy.readPng path
    either throwError (\case 
        Juicy.ImageRGBA8 img -> return img
        _ -> throwError $ "Could not decode image: " ++ path
        ) 
        imgEither
--}
bulkLoad :: (MonadIO m, MonadError Text m) => Seq FilePath -> m (Seq Picture)
bulkLoad = mapM loadPngAsBmp
