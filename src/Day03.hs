module Day03 (part1, part2) where

import Data.Char (isLower, ord, toLower)
import Data.Foldable (find)
import Data.String

newtype Compartment = Compartment String deriving (Show)

data Rucksack = Rucksack
  { left :: Compartment,
    right :: Compartment
  }
  deriving (Show)

findItemsInBothComparments :: Rucksack -> Maybe Char
findItemsInBothComparments (Rucksack (Compartment left) (Compartment right)) =
  find (`elem` right) left

part1 :: [String] -> String
part1 lines = show $sum $ map (getPriority . findItemsInBothComparments . createRucksack) lines

getPriority :: Maybe Char -> Int
getPriority Nothing = 0
getPriority (Just c)
  | isLower c = ord c - 96
  | otherwise = ord c - 38

createRucksack :: String -> Rucksack
createRucksack content =
  let n = length content `div` 2
      (left, right) = splitAt n content
   in Rucksack (Compartment left) (Compartment right)

part2 :: [String] -> String
part2 lines = show $ sum $ map (getPriority . findCommonItem) $ fromList 3  lines

findCommonItem :: [String] -> Maybe Char
findCommonItem (a:b:c:_) = 
    find (\x -> x `elem` b && x `elem` c) a

fromList :: Int -> [a] -> [[a]]
fromList n = foldr (\v a ->
    case a of
        (x:xs) -> if length x < n then (v:x):xs else [v]:a
        _ -> [[v]]
    ) []