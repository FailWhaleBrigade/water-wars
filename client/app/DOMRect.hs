module DOMRect where

import Miso

-- | Direct property access without JSON decoding
getBoundingRectProps :: JSVal -> IO (Maybe DomRect)
getBoundingRectProps rectVal = do
  -- Method 1: Using Miso's getProp with toJSString
  mx <- getProp "x" rectVal >>= fromJSVal
  my <- getProp "y" rectVal >>= fromJSVal
  mwidth <- getProp "width" rectVal >>= fromJSVal
  mheight <- getProp "height" rectVal >>= fromJSVal
  mtop <- getProp "top" rectVal >>= fromJSVal
  mleft <- getProp "left" rectVal >>= fromJSVal
  mright <- getProp "right" rectVal >>= fromJSVal
  mbottom <- getProp "bottom" rectVal >>= fromJSVal

  pure $ case (mx, my, mwidth, mheight, mtop, mleft, mright, mbottom) of
    (Just x, Just y, Just w, Just h, Just t, Just l, Just r, Just b) ->
      Just $
        DomRect
          { x = x
          , y = y
          , width = w
          , height = h
          , top = t
          , left = l
          , right = r
          , bottom = b
          }
    _ -> Nothing

data DomRect = DomRect
  { x :: !Double
  , y :: !Double
  , width :: !Double
  , height :: !Double
  , top :: !Double
  , left :: !Double
  , right :: !Double
  , bottom :: !Double
  }
  deriving (Show, Eq, Ord)
