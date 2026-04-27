module WaterWars.Client.Resources.Block (
  module WaterWars.Core.Terrain.Block,
  BlockMap,
  blocks,
  lookupBlockMap,
  loadBlockMap,
) where

import Control.Monad.IO.Class
import qualified Data.Foldable as Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics
import Miso.Prelude hiding ((.))
import WaterWars.Client.Codec.Resource
import WaterWars.Client.Resources.Image (GameImage)
import WaterWars.Core.Terrain.Block
import Data.Vector (Vector)
import qualified Data.Vector as Vector

newtype BlockMap = BlockMap {getBlockMap :: Vector GameImage}
  deriving (Generic, Eq, Show)

instance FromJSVal BlockMap where
  fromJSVal val = fmap (BlockMap . Vector.fromList) <$> fromJSVal val

instance ToJSVal BlockMap where
  toJSVal val = toJSVal $ Vector.toList $ getBlockMap val

lookupBlockMap :: BlockContent -> BlockMap -> GameImage
lookupBlockMap val bm = getBlockMap bm Vector.! fromEnum val

loadBlockMap :: (MonadIO m) => MisoString -> m BlockMap
loadBlockMap baseUrl = do
  loadedTextures <- bulkLoad $ fmap ((baseUrl <>) . toMisoString) blocks
  return . BlockMap . Vector.fromList $ Foldable.toList loadedTextures

blocks :: Seq FilePath
blocks =
  Seq.fromList
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
