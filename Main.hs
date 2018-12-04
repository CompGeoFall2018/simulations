{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Eriksson

import Data.Real.Constructible (Construct, fromConstruct)
import Data.JSString (JSString, unpack, pack)
import GHCJS.Marshal (fromJSVal, toJSVal)
import GHCJS.Nullable (maybeToNullable)
import GHCJS.Foreign.Callback (Callback, syncCallback1', OnBlocked(ContinueAsync))
import GHCJS.Types (JSVal, jsval)
import Debug.Trace (trace)

simplePoly :: Polygon
simplePoly = polygon [(0, 0), (4, 0), (4, 3), (2, 3), (2, 1), (0, 1)]

-- |convert a polygon to a list of pair of floating points, suitable for JS
fixupPoly :: [Point] -> [(Double, Double)]
fixupPoly = map (\(x, y) -> (fromConstruct x, fromConstruct y))

splitJS :: JSVal -> IO JSVal
splitJS jsval = do
    -- irrefutable, we should fix this
    -- to make sure we don't crash anything
    -- but this is being called from javascript so we don't care much
    Just (raw_pts :: [(Double, Double)]) <- fromJSVal jsval
    let pts :: [(Number, Number)]
        pts = map (\(x, y) -> (fromInteger $ round x, fromInteger $ round y)) raw_pts
    toJSVal $ fmap (\(poly1, poly2) -> map fixupPoly [ poly1, poly2 ]) $ split pts

foreign import javascript unsafe "splitPolygon = $1"
    js_set_splitPolygon :: JSVal -> IO ()

main :: IO ()
main = do
    callback <- syncCallback1' splitJS
    js_set_splitPolygon (jsval $ callback)
