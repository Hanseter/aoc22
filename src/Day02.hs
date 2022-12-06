module Day02 (part1, part2) where

import Data.String

data Gesture = Rock | Paper | Scissors deriving (Show, Eq)

data Outcome = Win | Draw | Loss deriving (Show, Eq)

data Round1 = Round1 Gesture Gesture deriving (Show)

data Round2 = Round2 Gesture Outcome deriving (Show)

part1 :: [String] -> String
part1 lines = show $ sum $ map scoreRound1 (toRounds lines)

toRounds :: [String] -> [Round1]
toRounds = map toRound

toRound :: String -> Round1
toRound line =
  let foo = map toGesture (words line)
      (theirs : mine : _) = foo
   in Round1 theirs mine

toGesture :: String -> Gesture
toGesture "A" = Rock
toGesture "X" = Rock
toGesture "B" = Paper
toGesture "Y" = Paper
toGesture "C" = Scissors
toGesture "Z" = Scissors

scoreRound1 :: Round1 -> Int
scoreRound1 round = scoreResult round + scorePick round

scoreResult :: Round1 -> Int
scoreResult (Round1 theirs mine)
  | theirs == mine = 3
  | theirs == Rock && mine == Paper = 6
  | theirs == Paper && mine == Scissors = 6
  | theirs == Scissors && mine == Rock = 6
  | otherwise = 0

scorePick :: Round1 -> Int
scorePick (Round1 _ mine) = scoreGesture mine

scoreGesture :: Gesture -> Int
scoreGesture Rock = 1
scoreGesture Paper = 2
scoreGesture Scissors = 3

part2 :: [String] -> String
part2 lines = show $ sum $ map scoreRound2 (toRounds2 lines)

toRounds2 :: [String] -> [Round2]
toRounds2 = map toRound2

toRound2 :: String -> Round2
toRound2 line =
  let (pick : outcome : _) = words line
   in Round2 (toGesture pick) (toOutcome outcome)

toOutcome :: String -> Outcome
toOutcome "X" = Loss
toOutcome "Y" = Draw
toOutcome "Z" = Win

scoreRound2 :: Round2 -> Int
scoreRound2 (Round2 pick outcome) = scoreOutcome outcome + scoreGesture (calcMyGesture outcome pick)

scoreOutcome :: Outcome -> Int
scoreOutcome Win = 6
scoreOutcome Draw = 3
scoreOutcome Loss = 0

calcMyGesture :: Outcome -> Gesture -> Gesture
calcMyGesture Win = winningGesture
calcMyGesture Draw = drawingGesture
calcMyGesture Loss = losingGesture

winningGesture :: Gesture -> Gesture
winningGesture Rock = Paper
winningGesture Paper = Scissors
winningGesture Scissors = Rock

drawingGesture :: Gesture -> Gesture
drawingGesture x = x

losingGesture :: Gesture -> Gesture
losingGesture Rock = Scissors
losingGesture Paper = Rock
losingGesture Scissors = Paper