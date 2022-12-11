module Main where

import qualified Day10 (part1, part2)
import Data.Char (ord)

main :: IO ()
main = do
  file <- readFile "/home/rene/projects/haskell/adventOfCode/res/day10.txt"
  let input = lines file
  putStrLn "Part 1"
  putStrLn $ Day10.part1 input
  putStrLn "Part 2"
  putStrLn $ Day10.part2 input
