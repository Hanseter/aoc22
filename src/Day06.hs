module Day06 (part1, part2) where

import Data.String
import Data.List (nub)

part1 :: [String] -> String
part1 [line] = show $ findPacketMarker line

findMarker :: Int -> Int -> String -> [Int]
findMarker _ _ [] = []
findMarker same n xs = let
          others = take same xs
          lOthers = length others
          ret = [n+same | (lOthers < same) || lOthers == length (nub others)]
          in ret ++ findMarker same (n+1) (tail xs)

findPacketMarker :: String -> [Int]
findPacketMarker = findMarker 4 0

findWordMarker :: String -> [Int]
findWordMarker = findMarker 14 0


part2 :: [String] -> String
part2 [line] = show $ findWordMarker line