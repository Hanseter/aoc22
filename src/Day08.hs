module Day08 (part1, part2) where

import Data.Char (digitToInt)
import Data.List (transpose)
import Data.String
import Debug.Trace (trace)

data Tree = Tree Int Bool 
instance Show Tree where
  show (Tree height visible) = show height

type TreeLine = [Tree]

part1 :: [String] -> String
part1 lines =
  let leftToRight = map (markVisibleTrees (-1)) $ parseTreeLines lines
      topToBottom = map (markVisibleTrees (-1)) $ transpose leftToRight
      bottomToTop = map (markVisibleTrees (-1) . reverse) topToBottom
      rightToLeft = map (markVisibleTrees (-1) . reverse) $ transpose bottomToTop
   in show $ length $ filter (\(Tree _ visible) -> visible) $ concat rightToLeft

parseTreeLines :: [String] -> [TreeLine]
parseTreeLines = map (map (\x -> Tree (digitToInt x) False))

markVisibleTrees :: Int -> TreeLine -> TreeLine
markVisibleTrees _ [] = []
markVisibleTrees maxHeight ((Tree height visible) : xs) =
  let higher = height > maxHeight
      newHeight = max height maxHeight
      newVisible = higher || visible
   in Tree height newVisible : markVisibleTrees newHeight xs

data Tree2 = Tree2
  { height :: Int,
    s :: Int
  } 

instance Show Tree2 where
  show (Tree2 h s) = "(" ++ show h ++ "," ++ show s ++ ")"

type TreeLine2 = [Tree2]

part2 :: [String] -> String
part2 lines =
  let leftToRight = map score $ parseTreeLines2 lines
      topToBottom = map score $ transpose leftToRight
      bottomToTop = map (score . reverse) topToBottom
      rightToLeft = map (score . reverse) $ transpose bottomToTop
   in show $ maximum $ map s $ concat rightToLeft

parseTreeLines2 :: [String] -> [TreeLine2]
parseTreeLines2 = map (map (\x -> Tree2 (digitToInt x) 1))

score :: TreeLine2 -> TreeLine2
score [] = []
score ((Tree2 h s) : xs) =
  let (visible, hidden) = span ((< h) . height) xs
      a = length visible + if null hidden then 0 else 1
   in Tree2 h (s * a) : score xs