module Day07 (part1, part2) where

import Data.List (elemIndex, sort, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.String
import Debug.Trace (trace)

class FileSystemNode a where
   size :: a -> Integer

data File = File String Integer deriving (Show)
instance FileSystemNode File where
   size (File _ size) = size

data Directory = Directory [String] [File] [Directory] Integer deriving (Show)
instance Eq Directory where
   (Directory nameA _ _ _ ) == (Directory nameB _ _ _) = nameA == nameB
instance Ord Directory where
   compare (Directory nameA _ _ _ ) (Directory nameB _ _ _) = compare (length nameA) (length nameB)
instance FileSystemNode Directory where
   size (Directory _ _ _ size) = size




part1 :: [String] -> String
part1 lines = let
   rootDir = initDirOrder $ reverse $ sort (parseFilesystem [] lines)
   subs = concatMap (\(Directory _ _ dirs _) -> flattenDirs dirs) rootDir
   in show $ sum $ map (\(Directory _ _ _ size) -> size) $ filter (\(Directory _ _ _ size) -> size <100000) subs 

parseFilesystem :: [String] -> [String] -> [Directory]
parseFilesystem _ [] = []
parseFilesystem currentDir commands =
  let (cds, rest) = break (== "$ ls" ) commands
      dirName = normalizeDirName $ currentDir ++ map (drop 5) cds
      (content, next) = break ((== "$ cd ") . take 5) (drop 1 rest)
      files = map parseFile $filter ((/= "dir") . take 3) content
   in Directory dirName files [] 0  : (parseFilesystem dirName next)

normalizeDirName :: [String] -> [String]
normalizeDirName xs= let
   (a,b) = break (=="..") xs
   in if null b then a else normalizeDirName $ init a++tail b

parseFile :: String -> File
parseFile file = let 
   (size, name) = break (== ' ') file
   in File (drop 1 name) (read size)

initDirOrder :: [Directory] -> [Directory]
initDirOrder [(Directory name files subDirs _)] = [Directory name files subDirs (calcSize files subDirs)]
initDirOrder xs = let
   currentDepth = length (path $ head xs)
   parentDepth = currentDepth-1
   (current, above) = span (\(Directory name _ _ _) -> length name == currentDepth) xs
   sizedCurrents = map (\(Directory name files subDirs _) -> Directory name files subDirs (calcSize files subDirs) ) current
   (parents, ancestors) = span (\(Directory name _ _ _) -> length name == parentDepth) above
   in initDirOrder $ (assignSubDirs parents sizedCurrents)++ancestors

path :: Directory -> [String]
path (Directory name _ _ _) = name

assignSubDirs :: [Directory] -> [Directory] -> [Directory]
assignSubDirs parents subs = map (\(Directory path files _ _) -> let
   childs = filter (\(Directory subPath _ _ _) ->  path `isPrefixOf` subPath) subs
   in Directory path files childs 0
   ) parents

calcSize :: [File] -> [Directory] -> Integer
calcSize files dirs =
   sum (map size files) + sum (map size dirs)

flattenDirs :: [Directory] -> [Directory]
flattenDirs [] = []
flattenDirs dirs = let 
   subs = concatMap (\(Directory _ _ sub _) -> sub) dirs
   in dirs ++ (flattenDirs subs)

part2 :: [String] -> String
part2 lines = let
   rootDir = initDirOrder $ reverse $ sort (parseFilesystem [] lines)
   freeSpace= 70000000 - (size $head rootDir)
   needed = 30000000 - freeSpace
   subs = concatMap (\  (Directory _ _ dirs _) -> flattenDirs dirs) rootDir
   sizes = map size $ filter ((>=needed) . size) subs 
   in show $ minimum sizes