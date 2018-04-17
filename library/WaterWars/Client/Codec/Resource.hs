module WaterWars.Client.Codec.Resource (loadPngAsBmp, bulkLoad, bulkLoadJpeg) where

import ClassyPrelude
import qualified Codec.Picture as Juicy
import Control.Monad.Error.Class

import Graphics.Gloss

loadPngAsBmp :: (MonadIO m, MonadError String m) => FilePath -> m Picture
loadPngAsBmp path = do
    imgEither <- liftIO $ Juicy.readPng path
    img       <- case imgEither of
        Left  msg -> throwError msg
        Right i   -> return i

    boolEither <- liftIO $ Juicy.writeDynamicBitmap (path ++ ".bmp") img
    isWritten  <- case boolEither of
        Left  msg -> throwError msg
        Right b   -> return b

    if not isWritten
        then throwError ("Could not write to File: " ++ show path)
        else liftIO (loadBMP (path ++ ".bmp"))

loadJpegAsBmp :: (MonadIO m, MonadError String m) => FilePath -> m Picture
loadJpegAsBmp path = do
    imgEither <- liftIO $ Juicy.readJpeg path
    img       <- case imgEither of
        Left  msg -> throwError msg
        Right i   -> return i

    boolEither <- liftIO $ Juicy.writeDynamicBitmap (path ++ ".bmp") img
    isWritten  <- case boolEither of
        Left  msg -> throwError msg
        Right b   -> return b

    if not isWritten
        then throwError ("Could not write to File: " ++ show path)
        else liftIO (loadBMP (path ++ ".bmp"))


bulkLoad :: (MonadIO m, MonadError String m) => Seq FilePath -> m (Seq Picture)
bulkLoad = mapM loadPngAsBmp

bulkLoadJpeg :: (MonadIO m, MonadError String m) => Seq FilePath -> m (Seq Picture)
bulkLoadJpeg = mapM loadJpegAsBmp
