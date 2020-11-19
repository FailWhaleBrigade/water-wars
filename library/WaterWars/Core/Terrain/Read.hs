module WaterWars.Core.Terrain.Read where

import           ClassyPrelude
import           WaterWars.Core.Game.Map
import           Data.Array.IArray
import           Data.List                                ( transpose )

readTerrainFromFile :: MonadIO m => FilePath -> m (Maybe Terrain)
readTerrainFromFile terrainFilePath = do
    content <- readFileUtf8 terrainFilePath
    let extractedTerrain =
            charMatrixToTerrain . map unpack . filter (not . isPrefixOf "#") . lines $ content
    case extractedTerrain of
        Left  err -> do
            putStrLn err
            pure Nothing
        Right x   -> do
            let Terrain a = x
            print $ bounds a
            return $ Just x


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
charToBlock '>' = SolidBlock LeftWall
charToBlock '<' = SolidBlock RightWall
charToBlock '.' = SolidBlock BottomRightCorner
charToBlock ',' = SolidBlock BottomLeftCorner
charToBlock '^' = SolidBlock TopRightCorner
charToBlock '"' = SolidBlock TopLeftCorner
charToBlock 'J' = SolidBlock EndLeft
charToBlock 'L' = SolidBlock EndRight
charToBlock _   = NoBlock
