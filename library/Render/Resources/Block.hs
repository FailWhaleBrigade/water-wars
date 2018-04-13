{-# LANGUAGE FlexibleContexts #-}
module Render.Resources.Block where

import ClassyPrelude
import Control.Monad.Error.Class

import qualified Graphics.Gloss as Gloss
import Render.Solid
import Render.Config

import Codec.Resource

type BlockMap = Map Block Gloss.Picture

data Block
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

setBlocks :: BlockMap -> Seq Solid
setBlocks blockmap = fromList
    (  mapMaybe
          (\x ->
              Solid blockSize blockSize (x, fieldHeight) <$> lookup Ceil blockmap
          )
          [-fieldWidth, (-fieldWidth + blockSize) .. fieldWidth]
        -- ceiling placement
    ++ mapMaybe
           (\y ->
               Solid blockSize blockSize (fieldWidth, y)
                   <$> lookup RightWall blockmap
           )
           [(fieldHeight - blockSize), (fieldHeight - blockSize * 2) .. -fieldHeight]
        -- right wall placement
    ++ mapMaybe
           (\x ->
               Solid blockSize blockSize (x, -fieldHeight)
                   <$> lookup Floor blockmap
           )
           [-fieldWidth, (-fieldWidth + blockSize) .. fieldWidth]
        -- floor placement
    ++ mapMaybe
           (\y ->
               Solid blockSize blockSize (-fieldWidth, y)
                   <$> lookup LeftWall blockmap
           )
           [-fieldHeight, (-fieldHeight + blockSize) .. fieldHeight]
        -- left wall placement
    ++ mapMaybe
           (\x -> Solid blockSize blockSize (x, -100) <$> lookup Floor blockmap)
           [-64, (-64 + blockSize) .. 64]
        -- platform
    ++ placeSingleBlock (-96)         (-100)         EndLeft           blockmap
    ++ placeSingleBlock 96            (-100)         EndRight          blockmap
    ++ placeSingleBlock fieldWidth    (-fieldHeight) BottomRightCorner blockmap -- bottom right corner placement
    ++ placeSingleBlock fieldWidth    fieldHeight    TopRightCorner    blockmap -- top right corner placement
    ++ placeSingleBlock (-fieldWidth) (-fieldHeight) BottomLeftCorner  blockmap -- bottom left corner placement
    ++ placeSingleBlock (-fieldWidth) fieldHeight    TopLeftCorner     blockmap -- top left corner placement
    ) 

placeSingleBlock :: Float -> Float -> Block -> BlockMap -> [Solid]
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




