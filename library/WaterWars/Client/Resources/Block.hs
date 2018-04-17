module WaterWars.Client.Resources.Block (module WaterWars.Core.Terrain.Block, BlockMap, placeSingleBlock, blocks, loadBlockMap) where

import ClassyPrelude
import Control.Monad.Error.Class

import qualified Graphics.Gloss as Gloss
import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config

import WaterWars.Client.Codec.Resource
import WaterWars.Core.Terrain.Block

type BlockMap = Map BlockContent Gloss.Picture


placeSingleBlock :: Float -> Float -> BlockContent -> BlockMap -> [Solid]
placeSingleBlock x y block blockmap =
    maybeToList (Solid blockSize blockSize (x, y) <$> lookup block blockmap)

loadBlockMap :: (MonadIO m, MonadError String m) => m BlockMap
loadBlockMap = do
    loadedTextures <- bulkLoad blocks
    return . mapFromList $ zip [Floor .. Ceil] (toList loadedTextures)

blocks :: Seq FilePath
blocks = fromList
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

