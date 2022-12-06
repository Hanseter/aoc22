module Day05 (part1, part2) where

import Data.List (elemIndex, transpose)
import Data.Maybe (fromMaybe)
import Data.String
import Debug.Trace (trace)

type Crate = Char --deriving(Show)

type Stack = [Crate] --deriving(Show)

data Command = Command
  { amount :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

part1 :: [String] -> String
part1 lines =
  let (stack, commands) = loadStacksAndCommands lines
   in show $ map head $ applyCommands stack commands

applyCommands :: [Stack] -> [Command] -> [Stack]
applyCommands = foldl applyCommandReverse

applyCommandReverse :: [Stack] -> Command -> [Stack]
applyCommandReverse stack (Command amount from to) =
  let (a, b) = splitAt (from-1) stack
      (remove:c) = b
      (transfer, keep) = splitAt amount remove
      stackAfterRemove = a++(keep:c)
      (e,f) = trace (show (to-1) ++"/" ++ show stackAfterRemove)splitAt (to-1) stackAfterRemove
      (add:g) = trace ("first:" ++ show e ++ " second:"++ show f) f
      stackAfterAdd = e++(((reverse transfer)++add):g)
   in stackAfterAdd

applyCommand :: [Stack] -> Command -> [Stack]

applyCommand stack (Command amount from to) =
  let (a, b) = splitAt (from-1) stack
      (remove:c) = b
      (transfer, keep) = splitAt amount remove
      stackAfterRemove = a++(keep:c)
      (e,f) = trace (show (to-1) ++"/" ++ show stackAfterRemove)splitAt (to-1) stackAfterRemove
      (add:g) = trace ("first:" ++ show e ++ " second:"++ show f) f
      stackAfterAdd = e++((transfer++add):g)
   in stackAfterAdd

loadStacksAndCommands :: [String] -> ([Stack], [Command])
loadStacksAndCommands lines =
  let index = fmap (`splitAt` lines) (elemIndex "" lines)
      (stacks, commands) = fromMaybe ([], []) index
   in (parseStacks stacks, parseCommands $ drop 1 commands)

parseStacks :: [String] -> [Stack]
parseStacks lines = buildStack (map (drop 1) (init lines))

buildStack :: [String] -> [Stack]
buildStack [] = []
buildStack lines
  | any null lines = []
  | otherwise =
    let heads = map head lines
        tails = map (drop 4) lines
        stack = filter (\x -> not (x == ' ')) heads
     in stack : buildStack tails

parseCommands :: [String] -> [Command]
parseCommands = map (parseCommand . words)

parseCommand :: [String] -> Command
parseCommand [_, amount, _, from, _, to] = Command (read amount) (read from) (read to)
parseCommand [_, amount, _, from, _, to, _] = Command (read amount) (read from) (read to)

part2 :: [String] -> String
part2 lines = let (stack, commands) = loadStacksAndCommands lines
   in show $ map head $ foldl applyCommand stack commands