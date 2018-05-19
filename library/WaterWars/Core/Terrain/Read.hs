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
    return $ charMatrixToTerrain . lines . unpack . decodeUtf8 $ content

-- TODO: make bounds generic
charMatrixToTerrain :: [[Char]] -> Terrain
charMatrixToTerrain x =
    Terrain $ listArray (BlockLocation (-8, -8), BlockLocation (8, 8))
        . map charToBlock
        . concat
        . transpose
        . reverse $ x

charToBlock :: Char -> Block
charToBlock 'x' = SolidBlock Middle
charToBlock '_' = SolidBlock Ceil
charToBlock '-' = SolidBlock Floor
charToBlock _ = NoBlock
