module N9
  (getSolutions9 
  ) where

extrapolateValue :: [Int] -> Int
extrapolateValue nums = sum $ last <$> difsSequence nums

extrapolateBackwards :: [Int] -> Int
extrapolateBackwards nums = foldr (-) 0 $ head <$> difsSequence nums

difsSequence :: [Int] -> [[Int]]
difsSequence nums = takeWhile (any (/= 0)) $ iterate genDif nums

genDif :: [Int] -> [Int]
genDif ls = zipWith (-) (tail ls) ls

parseFile :: String -> [[Int]]
parseFile file = map (read <$>) $ words <$> lines file

solution1 :: String -> Int
solution1 file = sum $ extrapolateValue <$> numSeqs
  where
    numSeqs = parseFile file

solution2 :: String -> Int
solution2 file = sum $ extrapolateBackwards <$> numSeqs
  where
    numSeqs = parseFile file

getSolutions9 :: String -> IO (Int, Int)
getSolutions9 filename = do
  file <- readFile filename
  return (solution1 file, solution2 file)
