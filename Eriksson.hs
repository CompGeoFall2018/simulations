{-# LANGUAGE TupleSections #-}

module Eriksson where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Real.Constructible
import Data.Monoid
import Control.Monad

type Number = Construct
type Point = (Number, Number)
type Segment = (Point, Point)
type Orientation = (Number, Number)
type Ray = (Point, Orientation)
type Polygon = [Segment]
type PolygonTraversal = Map.Map Point Ray
type Solution = ([Point], [Point])

rotate :: [a] -> [a]
rotate []     = []
rotate (x:xs) = xs ++ [x]

polygon :: [Point] -> Polygon
polygon ps = zip ps $ rotate ps

vertices :: Polygon -> [Point]
vertices = map fst

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
infixl 5 .+.

(*.) :: Number -> Point -> Point
c *. (x, y) = (c * x, c * y)
infixl 6 *.

(.-.) :: Point -> Point -> Point
p1 .-. p2 = p1 .+. (-1) *. p2
infixl 5 .-.

magnitude :: Segment -> Number
magnitude ((x1, y1), (x2, y2)) = sqrt $ (x2 - x1) ^ 2 + (y2 - y1) ^ 2

orientation :: Segment -> Orientation
orientation ((x1, y1), (x2, y2)) = (x2 - x1, y2 - y1)

(@+@) :: Orientation -> Orientation -> Orientation
(x1, y1) @+@ (x2, y2) = (x1 * x2 - y1 * y2, x2 * y1 + y2 * x1)
infixl 5 @+@

oNegate :: Orientation -> Orientation
oNegate (x, y) = (x, -y)

oReverse :: Orientation -> Orientation
oReverse (x, y) = (-x, -y)

normalize :: Orientation -> Orientation
normalize (x, y) = (x / m, y / m)
  where m = sqrt $ x ^ 2 + y ^ 2

oCompare :: Orientation -> Orientation -> Ordering
oCompare o1 o2 = if y1 >= 0
                   then if y2 < 0
                    then LT
                    else x2 `compare` x1
                   else if y2 >= 0
                     then GT
                     else x1 `compare` x2
  where (x1, y1) = normalize o1
        (x2, y2) = normalize o2

(@==@) :: Orientation -> Orientation -> Bool
o1 @==@ o2 = o1 `oCompare` o2 == EQ
infix 4 @==@

translate :: Number -> Ray -> Point
translate d (p, o) = d *. (normalize o) .+. p

addMidpoints :: Polygon -> Polygon
addMidpoints ss = concat $ zipWith3 aux ss ss' (rotate ss')
  where ss' = rotate ss
        aux s1 (a, b) s3 = if orientation s1 @==@ oReverse (orientation s3)
                             then let m = 1/2 *. a .+. 1/2 *. b in [(a, m), (m, b)]
                             else [(a, b)]

pairs :: Ord a => [a] -> [(a, a)]
pairs xs = [(x1, x2) | x1 <- xs, x2 <- xs, x1 < x2]

intersection :: Ray -> Segment -> Maybe Number
-- https://stackoverflow.com/a/565282
intersection (p, _r) (q, qPlusS) =
  let (vx, vy) `cross` (wx, wy) = vx * wy - vy * wx
      (vx, vy) `dot` (wx, wy) = vx * wx + vy * wy
      r = normalize _r
      s = qPlusS .-. q
      rxs = r `cross` s
      t = (q .-. p) `cross` s / rxs
      u = (q .-. p) `cross` r / rxs
  in if rxs == 0
       then if (q .-. p) `cross` r == 0
         then let t0 = (q .-. p) `dot` r / r `dot` r
                  t1 = t0 + s `dot` r / r `dot` r
                  t = if s `dot` r < 0 then t0 `max` t1 else t0 `min` t1
              in if 0 <= t then Just t else Nothing
         else Nothing
       else if 0 <= t && 0 <= u && u <= 1
         then Just t
         else Nothing

minBy :: Ord b => (a -> b) -> a -> a -> a
minBy f x y = if f x <= f y then x else y

rayTrace :: Polygon -> Ray -> Maybe (Number, Segment) -- distance along ray, edge
rayTrace pg (p, o) = uncons (mapMaybe aux pg) >>= Just . uncurry (foldr $ minBy fst)
  where aux e = fmap (,e) $ mfilter (>0) $ intersection (p, o) e

isClockwise :: Polygon -> Bool
isClockwise p = (>0) $ sum $ map (\((x1, y1), (x2, y2)) -> (x2 - x1) * (y2 + y1)) p

reversePolygon :: Polygon -> Polygon
reversePolygon p = reverse $ map (\(a, b) -> (b, a)) p

polygonTraversal :: Polygon -> PolygonTraversal
polygonTraversal = Map.fromList . map (\s -> (fst s, (fst s, orientation s)))

removeDegeneracy :: Polygon -> Polygon
removeDegeneracy p = foldr aux [] p
  where aux s  []      = if 0 < magnitude s then [s] else []
        aux s1 (s2:ss) =
          if 0 < magnitude s2 && not (orientation s1 @==@ orientation s2)
            then s1:s2:ss
            else (fst s1, snd s2):ss

tryPath :: Polygon -> PolygonTraversal -> Bool -> (Point, Point) -> Maybe Solution
tryPath pg t cw (p1, p2) =
  do r1 <- Map.lookup p1 t
     r2 <- Map.lookup p2 t
     (path1, path2) <- aux ([], []) False r1 r2
     return (vertices $ removeDegeneracy $ polygon path1, vertices $ removeDegeneracy $ polygon path2)
  where aux (path1, path2) hasLeftBorder r1@(p1', o1) r2@(p2', o2) =
          if not hasLeftBorder && (p1' == p2 || p2' == p1)
            then (p1':path1, p2':path2) <$ guard (p1' == p2 && p2' == p1)
            else if not (null path1) && (p1' == p1 || p2' == p2)
              then (path1, path2) <$ guard (p1' == p1 && p2' == p2)
              else do
                (d1, (_, v1)) <- rayTrace pg r1
                (d2, (_, v2)) <- rayTrace pg r2
                case d1 `compare` d2 of
                  LT -> let p1'' = translate d1 r1
                            p2'' = translate d1 r2
                            t = turn o1 p1'' v1
                            r1' = (p1'', o1 @+@ t)
                            r2' = (p2'', o2 @+@ t)
                        in aux (p1':path1, p2':path2) (hasLeftBorder || not (t @==@ (1, 0))) r1' r2'
                  GT -> let p1'' = translate d2 r1
                            p2'' = translate d2 r2
                            t = turn o2 p2'' v2
                            r1' = (p1'', o1 @+@ t)
                            r2' = (p2'', o2 @+@ t)
                        in aux (p1':path1, p2':path2) (hasLeftBorder || not (t @==@ (1, 0))) r1' r2'
                  EQ -> let p1'' = translate d1 r1
                            p2'' = translate d2 r2
                            t1 = turn o1 p1'' v1
                            t2 = turn o2 p2'' v2
                            t = t1 `sharpest` t2
                            r1' = (p1'', o1 @+@ t)
                            r2' = (p2'', o2 @+@ t)
                        in aux (p1':path1, p2':path2) (hasLeftBorder || not (t1 @==@ t2)) r1' r2'
                where
                  turn o p p' = if p == p' then (1,0) else orientation (p, p') @+@ oNegate o
                  o1' `sharpest` o2' =
                    let f = if cw then oNegate . oReverse else oReverse
                    in if f o1' `oCompare` f o2' == GT then o1' else o2'

split :: [Point] -> Maybe Solution
split ps = listToMaybe $ mapMaybe validate tries
  where _p = polygon ps
        cw = isClockwise _p
        p = addMidpoints _p
        p' = reversePolygon p
        try1 = tryPath p (polygonTraversal p) cw
        try2 = tryPath p' (polygonTraversal p') (not cw)
        tries = [try1, try2] <*> pairs (map fst p)
        validate Nothing           = Nothing
        validate (Just (ps1, ps2)) = (ps1, ps2) <$ guard ((all (liftM2 (||) (flip elem ps1) (flip elem ps2)) ps)
                                                          && all (liftM2 (||) (flip elem ps) (flip elem ps2)) ps1
                                                          && all (liftM2 (||) (flip elem ps) (flip elem ps1)) ps2)
