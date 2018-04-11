{-# LANGUAGE TypeFamilies #-}
module Render.Resources.Tiles (Tile, Tile(..), loadTileMap, setTiles) where

import ClassyPrelude
import qualified Graphics.Gloss as Gloss 
import Render.State

import Codec.Resource 

type TileMap = Map Tile Gloss.Picture

data Tile 
    = Floor
    | EndLeft
    | EndRight
    | BottomLeftCorner
    | BottomRightCorner
    | TopRightCorner
    | TopLeftCorner
    | LeftWall
    | RightWall
    | Middle
    | Ceil
    deriving (Show, Enum, Bounded, Eq, Ord, Read)

setTiles :: TileMap -> Seq Solid
setTiles tilemap =
    fromList ((mapMaybe (\x -> Solid 50 50 (x, 200) <$> lookup Ceil tilemap) $ [-200, (-200 + 32) .. 200])
    ++  maybeToList (Solid 50 50 (232, 200) <$> lookup TopRightCorner tilemap))

loadTileMap :: IO (Either String (TileMap))
loadTileMap = do
    loadedTextures <- bulkLoad tiles
    return $ do 
        textures <- loadedTextures
        Right (mapFromList (zip [Floor .. Ceil] (toList textures)))

tiles :: Seq (FilePath, Int, Int)
tiles =
    fromList
        [ ({- Floor,-} "resources/textures/block/block32.png", 32, 32)
        , ({- EndLeft,-} "resources/textures/block/blockendleft32.png", 32, 32)
        , ({- EndRight,-} "resources/textures/block/blockendright32.png", 32, 32)
        , ({- BottomLeftCorner,-} "resources/textures/block/bottomleftcornerblock32.png", 32, 32)
        , ({- BottomRightCorner,-} "resources/textures/block/bottomrightcornerblock32.png", 32, 32)
        , ({- TopRightCorner,-} "resources/textures/block/toprightcornerblock32.png", 32, 32)
        , ({- TopLeftCorner,-} "resources/textures/block/topleftcornerblock32.png", 32, 32)
        , ({- LeftWall,-} "resources/textures/block/leftwallblock32.png", 32, 32)
        , ({- RightWall,-} "resources/textures/block/rightwallblock32.png", 32, 32)
        , ({- Middle,-} "resources/textures/block/middleblock32.png", 32, 32)
        , ({- Ceil,-} "resources/textures/block/topblock32.png", 32, 32)
        ]
    