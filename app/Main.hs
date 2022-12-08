module Main where

import qualified Day07 (part1, part2)
import Data.Char (ord)

main :: IO ()
main = do
  file <- readFile "/home/rene/projects/haskell/adventOfCode/res/day07.txt"
  let input = lines file
  putStrLn "Part 1"
  putStrLn $ Day07.part1 input
  putStrLn "Part 2"
  putStrLn $ Day07.part2 input
