module Main where

import Eriksson

simplePoly :: Polygon
simplePoly = polygon [(0, 0), (4, 0), (4, 3), (2, 3), (2, 1), (0, 1)]

main :: IO ()
main = putStrLn $ show $ split simplePoly
