module Main where

import qualified Day06 (part1, part2)
import Data.Char (ord)

main :: IO ()
main = do
  file <- readFile "/home/rene/projects/haskell/adventOfCode/res/day06.txt"
  let input = lines file
  putStrLn "Part 1"
  putStrLn $ Day06.part1 input
  putStrLn "Part 2"
  putStrLn $ Day06.part2 input
