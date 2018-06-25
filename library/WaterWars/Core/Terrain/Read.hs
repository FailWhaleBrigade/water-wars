module WaterWars.Core.Terrain.Read where

import           ClassyPrelude
import           WaterWars.Core.Game.Map
import           System.IO                                ( openFile )
import           Data.Array.IArray
import           Data.List                                ( transpose )

readTerrainFromFile :: MonadIO m => FilePath -> m Terrain
readTerrainFromFile terrainFilePath = do
    terrainFile           <- liftIO $ openFile terrainFilePath ReadMode
    content :: ByteString <- hGetContents terrainFile
    let extractedTerrain =
            charMatrixToTerrain . lines . unpack . decodeUtf8 $ content
    case extractedTerrain of
        Left  err -> do
            putStrLn err
            fail ""
        Right x   -> do
            let Terrain a = x
            print $ bounds a
            return x


-- TODO: make bounds generic
charMatrixToTerrain :: [String] -> Either Text Terrain
charMatrixToTerrain x = do
    let height = length x
    when (height `mod` 2 == 0) $ Left "height has to be an odd number"
    let widths = map length x
    unless (allEqual widths) $ Left "not all lines have equal length"
    let width = headEx widths

    let maxX  = width `div` 2
    let maxY  = height `div` 2
    return
        $ Terrain
        $ listArray (BlockLocation (-maxX, -maxY), BlockLocation (maxX, maxY))
        . map charToBlock
        . concat
        . transpose
        . reverse
        $ x

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual (a : as) = all (== a) as

charToBlock :: Char -> Block
charToBlock 'x' = SolidBlock Middle
charToBlock '_' = SolidBlock Ceil
charToBlock '-' = SolidBlock Floor
charToBlock _   = NoBlock
