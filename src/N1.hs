module N1
    ( getSolutions1
    ) where

import System.Environment ()
import qualified Data.Text as T
import Data.List 
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
--import qualified Data.Text.Internal.Search as T.Internal.Search

type TextnumMap = M.Map T.Text Int
plainNumberMap :: TextnumMap
plainNumberMap = M.fromList [
                         ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8),
                         ("9", 9), ("0", 0)
                         ]

textNumberMap :: TextnumMap
textNumberMap = M.union  (M.fromList [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8),
                         ("nine", 9), ("zero", 0)])  plainNumberMap                                                 
                         
extNumberMap :: TextnumMap
extNumberMap = M.union (M.fromList capitalizedPairs) textNumberMap where
    capitalizedPairs = map (\(key, val) -> ((T.toUpper. T.take 1) key <> T.tail key, val)) $  M.toList textNumberMap

--currentNumberStrings :: TextnumMap
--currentNumberStrings = numberStringsExt

textToNum:: TextnumMap -> T.Text  -> Int
textToNum textnumMap key  = case M.lookup key textnumMap  of 
    (Just res) -> res
    Nothing -> 0

getNumOrNothing :: TextnumMap -> T.Text ->  Maybe Int
getNumOrNothing textnumMap text  = (textToNum textnumMap)  <$> find (`T.isPrefixOf` text) (M.keys textnumMap)


subtexts :: T.Text -> [T.Text]
subtexts "" = []
subtexts txt = txt : subtexts  (T.tail txt)

findAllNums' ::  TextnumMap -> T.Text -> [Int]
findAllNums' textnumMap text = foldl (\acc rest -> acc ++ (case getNumOrNothing textnumMap rest of 
                                                Nothing -> []
                                                (Just x) -> [x])) [] $ subtexts  text
                  
extractNum :: TextnumMap -> T.Text -> Int
extractNum textnumMap txt =  10 * firstNum + lastNum where
    firstNum = head res
    lastNum =  last res
    res =  findAllNums' textnumMap txt

getSum :: TextnumMap -> T.Text -> Int
getSum textnumMap file = sum nums  where
    --file <- TIO.readFile fileName
        fileLines = T.lines file
        nums = map (extractNum textnumMap) fileLines
    --return $ sum nums 

solution1 :: T.Text -> Int
solution1 = getSum plainNumberMap
solution2 :: T.Text -> Int
solution2 = getSum extNumberMap

getSolutions1:: String -> IO (Int, Int)
getSolutions1 filename = do  
    file <- TIO.readFile filename --getSum "Inputs/1.txt"
    return (solution1 file, solution2 file)

    