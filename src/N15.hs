module N15 
(getSolutions15
) where

import qualified Control.Monad.ST as ST
import Data.Array.ST
import Data.Char (ord)
import qualified Data.Array  as A
import ParsingFuncs (wordsWhen, splitOn)
type Label = (String, Int)
type LabelBox = [Label]
data Action = Remove | Replace deriving (Eq, Show)
data LabelInfo = LabelInfo {labelStr :: String, focalLength :: Int, box :: Int, action :: Action} deriving Show
 


hashAlg :: [Char] -> Int
hashAlg cList = foldl transFunc 0 cList where
  transFunc ::  Int -> Char -> Int
  transFunc curVal c =  ((curVal + ord c )* 17) `mod` 256

parseLabel :: String -> LabelInfo
parseLabel str 
        | '=' `elem` str = let [label, num] = splitOn '=' str in LabelInfo {labelStr = label, focalLength = read num, box = hashAlg label, action = Replace}   
        | '-' `elem` str = let label = takeWhile (/= '-') str in LabelInfo {labelStr = label, focalLength = 0, box = hashAlg label, action = Remove} 

modifyBox ::  LabelInfo -> LabelBox -> LabelBox
modifyBox labelInfo labelBox = let curLabel = labelStr labelInfo
                                   (start, end) = break ((curLabel ==).fst) labelBox in
  case action labelInfo of 
   Remove -> start ++ drop 1 end 
   Replace -> start ++ [(curLabel, focalLength labelInfo)] ++ drop 1 end  

updateArray ::  STArray s Int LabelBox -> String -> ST.ST s ()
updateArray ar curLabel = let labelInfo =  parseLabel curLabel  in
  modifyArray ar (box labelInfo) (modifyBox labelInfo)  

generateArray :: [String] -> A.Array Int LabelBox
generateArray codes = runSTArray $ do 
  ar <-  (newArray (0, 255) [])-- :: ST.ST s (STArray s Int LabelBox))
  mapM_ (updateArray ar) codes 
  return ar  
  
getCodes :: String -> [String]
getCodes file =  wordsWhen (== ',') $ filter (/= '\n') file

solution1 :: String -> Int
solution1 file = sum (hashAlg <$> getCodes file)

boxPower :: (Int, LabelBox) -> Int
boxPower (boxNum, labelBox) = (boxNum+1) * sum ( zipWith (\idx (_, fL) -> idx* fL) [1..] labelBox)

solution2 :: String -> Int
solution2 file =  let ar = generateArray (getCodes file)
                      arList = A.assocs ar in
                      sum $ map boxPower arList
                      
getSolutions15 :: String -> IO (Int, Int)
getSolutions15 filename = do
  file <- readFile filename
  --print $ generateArray (getCodes file)
  return (solution1 file, solution2 file)
