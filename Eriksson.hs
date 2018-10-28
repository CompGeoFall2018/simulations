module Eriksson where

import Data.List
import Data.Ratio

type Number = Rational
type Point = (Number, Number)
type Segment = (Point, Point)
type Orientation = (Number, Number)
type Polygon = [Segment]

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

(~+~) :: Segment -> Segment -> Segment
(a1, b1) ~+~ (a2, b2) = (a1 .+. a2, b1 .+. b2)
infixl 5 ~+~

(*~) :: Number -> Segment -> Segment
c *~ (a, b) = (c *. a, c *. b)
infixl 6 *~

(~+.) :: Segment -> Point -> Point
(a, b) ~+. p = a .+. (-1) *. b .+. p
infixl 5 ~+.

parallel :: Segment -> Segment -> Bool
parallel (a1, b1) (a2, b2) = x1 * y2 == x2 * y1
  where (x1, y1) = b1 .+. (-1) *. a1
        (x2, y2) = b2 .+. (-1) *. a2

pathStarts :: Polygon -> [Point]
pathStarts ss = concat $ zipWith3 aux ss ss' (rotate ss')
  where ss' = rotate ss
        aux s1 (a, b) s3 = a:(if parallel s1 s3 then [1 % 2 *. a .+. 1 % 2 *. b] else [])

pairs :: Eq a => [a] -> [(a, a)]
pairs xs = [(x1, x2) | x1 <- xs, x2 <- xs, x1 /= x2]

quadrance :: Segment -> Number
quadrance ((x1, y1), (x2, y2)) = (x2 - x1) ^ 2 + (y2 - y1) ^ 2

spread :: Segment -> Segment -> Number
spread s1 s2 = (a1*b2-a2*b1)^2/((a1^2+b1^2)*(a2^2+b2^2))
  where (a1, b1) = standardFormLineThroughOrigin s1
        (a2, b2) = standardFormLineThroughOrigin s2
        standardFormLineThroughOrigin ((x1, y1), (x2, y2)) = (y1 - y2, x2 - x1)
