module WaterWars.Client.Resources.Block (module WaterWars.Core.Terrain.Block, BlockMap, placeSingleBlock, blocks, lookupBlockMap, loadBlockMap) where

import WaterWars.Client.Render.Terrain.Solid
import WaterWars.Client.Render.Config

import WaterWars.Client.Codec.Resource
import WaterWars.Core.Terrain.Block
import Data.Map.Strict (Map)
import Control.Monad.IO.Class
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Foldable as Foldable
import WaterWars.Client.Resources.Image (GameImage)
import qualified Data.Text as Text
import GHC.Generics
import Miso.Prelude hiding ((.))

newtype BlockMap = BlockMap { getBlockMap :: Map BlockContent GameImage }
  deriving (Generic)

instance FromJSVal BlockMap where
    fromJSVal val = fmap (BlockMap . Map.mapKeys (read . fromMisoString)) <$> fromJSVal val

instance ToJSVal BlockMap where
    toJSVal val = toJSVal $ Map.mapKeys (toMisoString . Text.show) $ getBlockMap val


lookupBlockMap :: BlockContent -> BlockMap -> Maybe GameImage
lookupBlockMap val bm = Map.lookup val (getBlockMap bm)

placeSingleBlock :: Float -> Float -> BlockContent -> BlockMap -> [Solid]
placeSingleBlock x y block blockmap =
    Maybe.maybeToList (Solid blockSize blockSize (x, y) <$> lookupBlockMap block blockmap)

loadBlockMap :: (MonadIO m) => MisoString -> m BlockMap
loadBlockMap baseUrl = do
    loadedTextures <- bulkLoad $ fmap ((baseUrl  <>) . toMisoString) blocks
    return . BlockMap . Map.fromList $ zip [Floor .. Ceil] (Foldable.toList loadedTextures)

blocks :: Seq FilePath
blocks = Seq.fromList
    [ {- Floor -}
      "textures/block/block32.png"
    , {- EndLeft -}
      "textures/block/blockendleft32.png"
    , {- EndRight -}
      "textures/block/blockendright32.png"
    , {- BottomLeftCorner -}
      "textures/block/bottomleftcornerblock32.png"
    , {- BottomRightCorner -}
      "textures/block/bottomrightcornerblock32.png"
    , {- TopRightCorner -}
      "textures/block/toprightcornerblock32.png"
    , {- TopLeftCorner -}
      "textures/block/topleftcornerblock32.png"
    , {- LeftWall -}
      "textures/block/leftwallblock32.png"
    , {- RightWall -}
      "textures/block/rightwallblock32.png"
    , {- Middle -}
      "textures/block/middleblock32.png"
    , {- Ceil -}
      "textures/block/topblock32.png"
    ]
