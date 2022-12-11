module Main where

import qualified Day09 (part1, part2)
import Data.Char (ord)

main :: IO ()
main = do
  file <- readFile "/home/rene/projects/haskell/adventOfCode/res/day09.txt"
  let input = lines file
  putStrLn "Part 1"
  putStrLn $ Day09.part1 input
  putStrLn "Part 2"
  putStrLn $ Day09.part2 input
