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
type Polygon = [Segment]
type PolygonTraversal = Map.Map Point Point
type Solution = (Polygon, Polygon)

rotate :: [a] -> [a]
rotate []     = []
rotate (x:xs) = xs ++ [x]

polygon :: [Point] -> Polygon
polygon ps = zip ps $ rotate ps

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

(@==@) :: Orientation -> Orientation -> Bool
o1 @==@ o2 = normalize o1 == normalize o2
infix 4 @==@

translate :: Number -> Orientation -> Point -> Point
translate d o p = d *. (normalize o) .+. p

matchStep :: Orientation -- Path 1 current orientation
             -> Segment -- Path 1 next segment
             -> Orientation -- Path 2 current orientation
             -> Point -- Path 2 current point
             -> Point --  Path 2 next point
matchStep o1 s1 o2 p2 = translate d o2' p2
  where d = magnitude s1
        o2' = o2 @+@ (orientation s1 @+@ oNegate o1)

addMidpoints :: Polygon -> Polygon
addMidpoints ss = concat $ zipWith3 aux ss ss' (rotate ss')
  where ss' = rotate ss
        aux s1 (a, b) s3 = if orientation s1 @==@ oReverse (orientation s3)
                             then let m = 1/2 *. a .+. 1/2 *. b in [(a, m), (m, b)]
                             else [(a, b)]

pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [(x1, x2) | x1 <- xs, x2 <- xs, x1 /= x2]

intersection :: Segment -> Segment -> [Point]
-- https://stackoverflow.com/a/565282
intersection (p, pPlusR) (q, qPlusS) =
  let (vx, vy) `cross` (wx, wy) = vx * wy - vy * wx
      (vx, vy) `dot` (wx, wy) = vx * wx + vy * wy
      r = pPlusR .-. p
      s = qPlusS .-. q
      rxs = r `cross` s
      t = (q .-. p) `cross` s / rxs
      u = (q .-. p) `cross` r / rxs
      btwn01 x = 0 <= x && x <= 1
      atT t' = p .+. t' *. r
  in if rxs == 0
       then if (q .-. p) `cross` r == 0
         then
           let t0 = (q .-. p) `dot` r / r `dot` r
               t1 = t0 + s `dot` r / r `dot` r
           in if btwn01 t0
                then if btwn01 t1
                  then [atT t0, atT t1]
                  else [atT t0]
                else if btwn01 t1
                  then [atT t1]
                  else []
         else []
       else if btwn01 t && btwn01 u
         then [atT t]
         else []

offSegment :: Segment -> Point -> Bool
offSegment s p = null $ intersection s (p, p)

trimToIntersection :: Polygon -> Segment -> Segment
trimToIntersection p s = minimumBy (comparing magnitude)
                         $ map (<$ s)
                         $ concatMap (intersection s) p

reversePolygon :: Polygon -> Polygon
reversePolygon p = reverse $ map (\(a, b) -> (b, a)) p

polygonTraversal :: Polygon -> PolygonTraversal
polygonTraversal = Map.fromList

removeDegeneracy :: Polygon -> Polygon
removeDegeneracy p = foldr aux [] p
  where aux s      []          = [s]
        aux (a, _) ((b, c):ss) = if offSegment (a, c) b
                                   then (a,b):(b,c):ss
                                   else (a,c):ss

tryPath :: Polygon -> PolygonTraversal -> (Point, Point) -> Maybe Solution
-- initial, incomplete implementation; only follows border of polygon
tryPath pg t (p1, p2) = do o1 <- initOrientation p1
                           o2 <- initOrientation p2
                           aux p1 o1 p2 o2 ([p1], [p2])
  where initOrientation p = orientation . (p,) <$> Map.lookup p t
        aux p1' o1' p2' o2' (path1, path2) =
          if p1' == p2
            then if p2' == p1
              then Just (removeDegeneracy $ polygon path1, removeDegeneracy $ polygon path2)
              else Nothing
            else do p1'' <- Map.lookup p1' t
                    p2'' <- Map.lookup p2' t
                    let s1 = (p1', p1'')
                        s2 = (p2', p2'')
                    guard $ p2'' == matchStep o1' s1 o2' p2'
                    aux p1'' (orientation s1) p2'' (orientation s2) (p1'':path1, p2'':path2)

split :: Polygon -> Maybe Solution
split _p = getFirst $ mconcat $ map First tries
  where p = addMidpoints _p
        p' = reversePolygon p
        try1 = tryPath p (polygonTraversal p)
        try2 = tryPath p' (polygonTraversal p')
        tries = [try1, try2] <*> pairs (map fst p)
