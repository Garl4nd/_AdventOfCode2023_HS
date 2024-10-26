module N13
  ( getSolutions13
  ) where

import qualified Data.Array as A
import Data.Array ((!))
import ParsingFuncs (splitBySubstr, countIf, splitOn)

type CharGrid = A.Array (Int, Int) Char

strToIntGrid :: String -> CharGrid
strToIntGrid str = A.listArray ((0, 0), (numLines -1, lineSize-1)) $ concat ls  where
  ls = lines str
  numLines = length ls
  lineSize = length $ head ls


parsePuzzle :: String -> [CharGrid]
parsePuzzle file = strToIntGrid <$> splitBySubstr "\n\n" file


getScore :: Int-> CharGrid ->   Int
getScore errCount ar  = case filter ( (==errCount).snd) $ findReflectionsAlong X ar of 
  [(idx, _)] -> idx
  _ -> case filter ( (==errCount).snd) $ findReflectionsAlong Y ar of 
    [(idx,_)] -> 100*idx
    _ -> 0

findReflectionsAlong :: Dir -> CharGrid -> [(Int, Int)]
findReflectionsAlong dir ar = findReflectionsAlong' minInd initialCandidates where
  ((ymin, xmin), (ymax, xmax)) = A.bounds ar
  initialCandidates = zip (if dir == X then [xmin+1..xmax] else [ymin+1..ymax]) (repeat 0)
  (minInd, maxInd) = if dir == X then (ymin, ymax) else (xmin, xmax)  
  findReflectionsAlong' :: Int -> [(Int, Int)] -> [(Int, Int)]
  findReflectionsAlong' _ []  = []
  findReflectionsAlong' curInd remainingPlanes 
    | curInd > maxInd = remainingPlanes
    | otherwise = findReflectionsAlong' (curInd+1) $  findReflectionsAt ar curInd remainingPlanes
  findReflectionsAt :: CharGrid  -> Int -> [(Int, Int)]->  [(Int, Int)]
  findReflectionsAt arr atInd candidates = filter  (\(_, remErrors) -> remErrors <= 1)  (map (numberOfErrors dir) candidates)  where
    numberOfErrors :: Dir -> (Int, Int) -> (Int, Int)
    numberOfErrors X (x,numOfErrors) =  (x, numOfErrors + countIf (\(x_l, x_u) -> arr ! (atInd, x_l) /= arr ! (atInd, x_u) )  (zip [x-1,x-2..xmin] [x..xmax]))
    numberOfErrors Y (y,numOfErrors)  = (y, numOfErrors + countIf (\(y_l, y_u) -> arr ! (y_l, atInd) /= arr ! (y_u, atInd) )  (zip [y-1,y-2..ymin] [y..ymax]))
data Dir = X | Y deriving (Show, Eq)

solution1 :: String -> Int
solution1 file = if 0 `elem` scores then 0 else sum scores where
  ars = parsePuzzle file
  scores = getScore 0 <$> ars

solution2 :: String -> Int
solution2 file = if 0 `elem` scores then 0 else sum scores where
  ars = parsePuzzle file
  scores = getScore 1 <$> ars

getSolutions13 :: String -> IO (Int, Int)
getSolutions13 filename = do 
  file <- readFile filename    
  return (solution1 file ,solution2 file)