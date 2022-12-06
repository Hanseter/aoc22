module Day04 (part1, part2) where

import Data.Char (ord)
import Data.String

data Range = Range Int Int deriving (Show)

part1 :: [String] -> String
part1 lines = show $ length $ filter (\(a,b) -> contains a b || contains b a)$ map rangePair lines

rangePair :: String -> (Range, Range)
rangePair line =
  let foo = split ',' line
      (a : b : _) = foo
      (minA : maxA : _) = split '-' a
      (minB : maxB : _) = split '-' b
   in (Range (read minA) (read maxA), Range (read minB) (read maxB))

split :: Char -> String -> [String]
split _ [] = [""]
split delimiter (c : cs)
  | c == delimiter = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split delimiter cs

contains :: Range -> Range -> Bool
contains (Range minA maxA) (Range minB maxB) = minA <= minB && maxA >= maxB

part2 :: [String] -> String
part2 lines = show $ length $ filter (\(a,b) -> overlaps a b || overlaps b a)$ map rangePair lines

overlaps :: Range -> Range -> Bool
overlaps (Range minA maxA) (Range minB maxB) = (minB >= minA  && minB <= maxA) || (maxB >= minA && maxB <= maxA)
