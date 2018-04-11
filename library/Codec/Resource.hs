module Codec.Resource (loadPngAsBmp, Height, Width, bulkLoad) where

import ClassyPrelude
import Graphics.Gloss
import Codec.Picture.Repa

type Width = Int
type Height = Int

loadPngAsBmp :: FilePath -> Width -> Height -> IO (Either String Picture)
loadPngAsBmp = readPng
    
-- taken from https://stackoverflow.com/questions/12222728/png-to-bmp-in-haskell-for-gloss
readPng :: FilePath -> Int -> Int -> IO (Either String Picture)
readPng path w h = do
  imgEither <- readImageRGBA path
  return $ do 
    img <- imgEither
    let bs = toByteString . reverseColorChannel $ onImg flipVertically img
    Right (bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True)

bulkLoad :: Seq (FilePath, Width, Height) -> IO (Either String (Seq Picture))
bulkLoad infos = do
    loading <- mapM (\(fp, width, height) -> loadPngAsBmp fp width height) infos :: IO (Seq (Either String Picture))
    let loaded = sequenceA loading :: Either String (Seq Picture)
    return loaded