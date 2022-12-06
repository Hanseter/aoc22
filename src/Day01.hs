module Day01 (part1, part2) where

import Data.String
import Data.List

part1 :: [String] -> String
part1 xs = show $ head (calcSums xs)

calcSums :: [String] -> [Integer]
calcSums lines = 
    let
      elves = splitAtEmpty lines
      elvesAsInts = map (map (\x -> read x ::Int)) elves
      ret = reverse $ sort $ map sum elvesAsInts
    in map toInteger ret


splitAtEmpty :: [String] -> [[String]]
splitAtEmpty xs = 
    let (ret, _, _) = splitEmptyInternal [[]] [] xs
    in ret

splitEmptyInternal :: [[String]] -> [String] -> [String] -> ([[String]], [String], [String])
splitEmptyInternal ret acc [] = (ret, [], [])
splitEmptyInternal ret acc (x:xs)
    | null x = splitEmptyInternal (acc:ret) [] xs
    | otherwise = splitEmptyInternal ret (x:acc) xs

readElves :: [String] -> [Integer]
readElves [] = []
readElves (x:xs) = [read x] 

part2 :: [String] -> String
part2 lines = show $ sum $ take 3 (calcSums lines)