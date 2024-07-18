module N6
  ( getSolutions6
  ) where
import ParsingFuncs (splitOn, readStrList)


type Time = Int
type Distance = Int
distance :: Time -> Time -> Distance
distance waitTime maxTime = (maxTime - waitTime) * waitTime  

limitingTimes ::Time-> Distance -> (Maybe Double, Maybe Double)
limitingTimes maxTime recordDist 
   | disc <0 = (Nothing, Nothing)
   | disc == 0 = (Just $ fromIntegral maxTime/2, Nothing)
   | otherwise = (Just $ (fromIntegral maxTime - discSqrt )/2, Just $ (fromIntegral maxTime + discSqrt)/2) where
      disc = maxTime^2 - 4*recordDist
      discSqrt = (sqrt.fromIntegral) disc
    
sufficientTimes :: Time -> Distance -> [Time]
sufficientTimes maxTime recordDist = case limitingTimes maxTime recordDist of
     (Just t1, Just t2) -> [ceiling t1..floor t2]
     (Just t, Nothing) -> if (ceiling t== floor t) then [floor t] else []
     _                  -> []

strsFromLine :: String -> [String]
strsFromLine file = splitOn ' ' $ last $ splitOn ':' file

parseFileFor1 :: String -> ([Time], [Distance])
parseFileFor1 file = (read <$> strsFromLine timeLine, read <$> strsFromLine distLine) where
  timeLine:distLine:_ = lines file

parseFileFor2 :: String -> (Time, Distance)
parseFileFor2 file = (read (concat $ strsFromLine timeLine), read (concat $ strsFromLine distLine)) where
  timeLine:distLine:_ = lines file

solution1 :: String -> Int
solution1 file = product $ length <$> zipWith sufficientTimes maxTimes recordDists where
  (maxTimes, recordDists) = parseFileFor1 file

solution2 :: String -> Int
solution2 file = length $  sufficientTimes maxTime recordDist where
  (maxTime, recordDist) = parseFileFor2 file
  
getSolutions6 :: String -> IO (Int, Int)
getSolutions6 filename = do
  file <- readFile filename
  print $ parseFileFor1 file
  print $ parseFileFor2 file
  return (solution1 file, solution2 file)
