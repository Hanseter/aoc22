module Day09 (part1, part2) where

import Data.List (nub)
import Data.String
import Debug.Trace (trace)

data Point = Point Int Int deriving (Show, Eq)

data Instruction = Instruction Point Int

part1 :: [String] -> String
part1 lines = show $ length $ nub $ moveRope (parseInstructions lines) $ take 2 (repeat (Point 0 0))

parseInstructions :: [String] -> [Instruction]
parseInstructions = map parseInstruction

parseInstruction :: String -> Instruction
parseInstruction (dir : _ : count) = Instruction (parseDir dir) (read count)

parseDir :: Char -> Point
parseDir 'U' = Point 0 1
parseDir 'D' = Point 0 (-1)
parseDir 'L' = Point (-1) 0
parseDir 'R' = Point 1 0

moveRope :: [Instruction] -> [Point] -> [Point]
moveRope [] knots = [last knots]
moveRope ((Instruction _ 0) : xs) knots = moveRope xs knots
moveRope ((Instruction dir count) : xs) (h : knots) =
  let nH = add dir h
      nT = move (nH:knots)
   in (last nT):moveRope (Instruction dir (count -1) : xs) nT

move :: [Point] -> [Point]
move [] = []
move [x] = [x]
move (x:y:xs) = let 
     moved = moveTail x y
     in x:move (moved:xs)

   

add :: Point -> Point -> Point
add (Point ax ay) (Point bx by) = Point (ax + bx) (ay + by)

-- sub :: Point -> Point -> Point
-- sub (Point ax ay) (Point bx by) = Point (ax-bx) (ay-by)

moveTail :: Point -> Point -> Point
moveTail (Point hx hy) (Point tx ty) =
  let vx = hx - tx
      vy = hy - ty
   in if vx > 1 || vx < (-1) || vy > 1 || vy < (-1) then Point (tx + (clamp (-1) vx 1)) (ty + (clamp (-1) vy 1)) else Point tx ty

clamp :: (Ord a) => a -> a -> a -> a
clamp lower x upper = min (max x lower) upper

-- moveXY :: Int -> Int -> Int
-- moveXY h t
--  | h== t =t
--  | h> t =

part2 :: [String] -> String
part2 lines = show $ length $ nub $ moveRope (parseInstructions lines) $ take 10 (repeat (Point 0 0))