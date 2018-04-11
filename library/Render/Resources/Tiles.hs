{-# LANGUAGE TypeFamilies #-}
module Render.Resources.Tiles (Tile(..), loadTileMap, setTiles) where

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

fieldWidth :: Float
fieldWidth = 256

fieldHeight :: Float
fieldHeight = 256

tileSize :: Float
tileSize = 32

setTiles :: TileMap -> Seq Solid
setTiles tilemap = fromList
    (  mapMaybe
          (\x ->
              Solid tileSize tileSize (x, fieldHeight) <$> lookup Ceil tilemap
          )
          [-fieldWidth, (-fieldWidth + tileSize) .. fieldWidth]
        -- ceiling placement
    ++ mapMaybe
           (\y ->
               Solid tileSize tileSize (fieldWidth, y)
                   <$> lookup RightWall tilemap
           )
           [(fieldHeight - tileSize), (fieldHeight - tileSize * 2) .. -fieldHeight]
        -- right wall placement
    ++ mapMaybe
           (\x ->
               Solid tileSize tileSize (x, -fieldHeight)
                   <$> lookup Floor tilemap
           )
           [-fieldWidth, (-fieldWidth + tileSize) .. fieldWidth]
        -- floor placement
    ++ mapMaybe
           (\y ->
               Solid tileSize tileSize (-fieldWidth, y)
                   <$> lookup LeftWall tilemap
           )
           [-fieldHeight, (-fieldHeight + tileSize) .. fieldHeight]
        -- left wall placement
    ++ mapMaybe
           (\x -> Solid tileSize tileSize (x, -100) <$> lookup Floor tilemap)
           [-64, (-64 + tileSize) .. 64]
        -- platform
    ++ placeSingleTile (-96)         (-100)         EndLeft           tilemap
    ++ placeSingleTile 96            (-100)         EndRight          tilemap
    ++ placeSingleTile fieldWidth    (-fieldHeight) BottomRightCorner tilemap -- bottom right corner placement
    ++ placeSingleTile fieldWidth    fieldHeight    TopRightCorner    tilemap -- top right corner placement
    ++ placeSingleTile (-fieldWidth) (-fieldHeight) BottomLeftCorner  tilemap -- bottom left corner placement
    ++ placeSingleTile (-fieldWidth) fieldHeight    TopLeftCorner     tilemap -- top left corner placement
    ) 

placeSingleTile :: Float -> Float -> Tile -> TileMap -> [Solid]
placeSingleTile x y tile tilemap =
    maybeToList (Solid tileSize tileSize (x, y) <$> lookup tile tilemap)

loadTileMap :: IO (Either String TileMap)
loadTileMap = do
    loadedTextures <- bulkLoad tiles
    return $ do
        textures <- loadedTextures
        Right (mapFromList (zip [Floor .. Ceil] (toList textures)))

tiles :: Seq FilePath
tiles = fromList
    [ {- Floor -}
      "resources/textures/block/block32.png"
    , {- EndLeft -}
      "resources/textures/block/blockendleft32.png"
    , {- EndRight -}
      "resources/textures/block/blockendright32.png"
    , {- BottomLeftCorner -}
      "resources/textures/block/bottomleftcornerblock32.png"
    , {- BottomRightCorner -}
      "resources/textures/block/bottomrightcornerblock32.png"
    , {- TopRightCorner -}
      "resources/textures/block/toprightcornerblock32.png"
    , {- TopLeftCorner -}
      "resources/textures/block/topleftcornerblock32.png"
    , {- LeftWall -}
      "resources/textures/block/leftwallblock32.png"
    , {- RightWall -}
      "resources/textures/block/rightwallblock32.png"
    , {- Middle -}
      "resources/textures/block/middleblock32.png"
    , {- Ceil -}
      "resources/textures/block/topblock32.png"
    ]




