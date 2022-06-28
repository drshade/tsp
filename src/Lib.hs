{-# LANGUAGE InstanceSigs #-}

module Lib where
import TspFile
import Control.Parallel.Strategies
import Data.Function.Memoize

class Measurable p where
    distance :: p -> p -> Float

-- Probably could memoize cleverly (e.g. distance between two coords - either direction pre-computed)
instance Measurable Coord where
    distance :: Coord -> Coord -> Float
    distance (Coord _ x1 y1) (Coord _ x2 y2) = sqrt $ ((x1 - x2) ** 2) + ((y1 - y2) ** 2)

-- This is probably really terrible performance
remove :: [c] -> Int -> [c]
remove cs idx = take idx cs ++ drop (idx + 1) cs

-- Bruteforce approach to TSP - try every path and select the minimum
bruteforce :: Measurable c => [c] -> Float
bruteforce [] = 0.0
bruteforce cs = bruteforce' 0.0 (head cs) (tail cs)

bruteforce' :: Measurable c => Float -> c -> [c] -> Float
bruteforce' acc me [] = acc
bruteforce' acc me cands =
    minimum $ parMap rseq (\(idx, c) -> bruteforce' (acc + distance me c) c (remove cands idx)) (zip [0..] cands)