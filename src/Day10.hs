module Day10 (part1, part2) where

import Data.String
import Data.List
import Data.Type.Coercion (sym)

data Instruction = Instruction Int Int deriving (Show)

part1 :: [String] -> String
part1 lines = show $sum $ analyzeSignalStrength 20 $ performInstructions 1 $ parseInstructions lines

analyzeSignalStrength :: Int -> [Int] -> [Int]
analyzeSignalStrength n signals = analyzeSignalStrength' n (drop (n -1) signals)

analyzeSignalStrength' :: Int -> [Int] -> [Int]
analyzeSignalStrength' n [] = []
analyzeSignalStrength' n (x : xs) =
  let strength = x * n
   in strength : analyzeSignalStrength' (n + 40) (drop 39 xs)

performInstructions :: Int -> [Instruction] -> [Int]
performInstructions register [] = []
performInstructions register ((Instruction 1 n) : xs) = register : performInstructions (n + register) xs
performInstructions register ((Instruction count n) : xs) = register : performInstructions register ((Instruction (count -1) n) : xs)

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction "noop" = Instruction 1 0
parseInstruction line = Instruction 2 (read (drop 5 line))

part2 :: [String] -> String
part2 lines = show $ evaluateSignal $ performInstructions 1 $ parseInstructions lines

evaluateSignal:: [Int] -> [String]
evaluateSignal [] = []
evaluateSignal signal = map (evaluateLine 0) $ chunksOf 40 signal

evaluateLine:: Int -> [Int] -> String
evaluateLine _ [] = []
evaluateLine n (current:rest) = let
    symbol = if n == current || n == (current -1) || n == current + 1 then '#' else '.'
    in symbol:evaluateLine (n+1) rest

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = foldr (\v a ->
    case a of
        (x:xs) -> if length x < n then (v:x):xs else [v]:a
        _ -> [[v]]
    ) []