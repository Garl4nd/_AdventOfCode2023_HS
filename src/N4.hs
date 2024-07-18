module N4
  ( getSolutions4
  ) where

import ParsingFuncs (readStrList, splitOn)

--import qualified Data.Text as T
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Function ((&))

--import GHC.Arr (listArray)
type Card = ([Int], [Int])

data IdxCard = IdxCard
  { idx :: Int
  , card :: Card
  }

fileToCards :: String -> [Card]
fileToCards file = map lineToCard $ lines file
  where
    lineToCard line =
      case splitOn '|' (splitOn ':' line !! 1) of
        [winningNumbersStr, actualNumbersStr] ->
          (readStrList ' ' winningNumbersStr, readStrList ' ' actualNumbersStr)
        _ -> ([], [])

countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

matches :: Card -> Int
matches card = countIf (`elem` winningNumbers) actualNumbers
  where
    (winningNumbers, actualNumbers) = card

pointsOnCard :: Card -> Int
pointsOnCard = matchesToPoints . matches
  where
    matchesToPoints :: Int -> Int
    matchesToPoints x
      | x > 0 = 2 ^ (x - 1)
      | otherwise = 0

points :: [IdxCard] -> IdxCard -> Int
points cards (IdxCard {idx = idx, card = card}) =
  1 + sum (map (points cards) copiedCards)
  where
    copiedCards = take (matches card) $ drop idx cards

allPoints :: [Card] -> Int
allPoints cards = sum (map (points idxCards) idxCards)
  where
    idxCards = cardsToIdxCards cards

points' :: IntArray -> Int -> Int
points' matchArray idx = 1 + sum (map (points' matchArray) copyIdxs)
  where
    copyIdxs = [idx + 1 .. (min ub idx + matches)]
    (_, ub) = A.bounds matchArray
    matches = matchArray ! idx

allPoints' :: [Card] -> Int
allPoints' cards = sum (map (points' matchArray) idxs)
  where
    matchArray = calcMatchAr cards
    idxs = [1 .. snd $ A.bounds matchArray]

pointsM :: IntArray -> Int -> Int
pointsM matchArray = pointsMemo
  where
    pointsArM :: (Int -> Int) -> Int -> Int
    pointsArM f idx = f $ 1 + sum (map (pointsM matchArray) copyIdxs)
      where
        copyIdxs = [idx + 1 .. (min ub idx + idxMatches)]
        (_, ub) = A.bounds matchArray
        idxMatches = matchArray ! idx
    memoize :: (Int -> a) -> (Int -> a)
    memoize f = (map f [0 ..] !!)
    fix :: (a -> a) -> a
    fix f =
      let x = f x
       in x
    pointsMemo :: Int -> Int
    pointsMemo = fix (memoize . pointsArM)

allPoints'M :: [Card] -> Int
allPoints'M cards = sum (map (pointsM matchArray) idxs)
  where
    matchArray = calcMatchAr cards
    idxs = [1 .. snd $ A.bounds matchArray]

allPointsC :: [Card] -> Int
allPointsC cards = calcImpl initialAr
  where
    matchAr = calcMatchAr cards
    (lb, ub) = A.bounds matchAr
    initialAr = A.listArray (1, length cards) $ repeat 1
    calcImpl :: IntArray -> Int
    calcImpl currentAr =
      case sum currentAr of
        0 -> 0
        x -> x + calcImpl updatedAr
          where updatedAr = foldl update currentAr [lb .. ub]
            --update ar idx = ar  // ((idx, (ar ! idx ) - curVal ) : [(i, (ar ! i) + curVal ) | i<- [idx +1..idx+matches]]) where
                update ar i =
                  A.accum (&) ar
                    $ (i, subtract oldValAtIdx)
                        : [ (j, (+ oldValAtIdx))
                          | j <- [i + 1 .. i + matchesForIdx]
                          ]
            --update ar idx = A.accum (+) ar $ (idx, -oldValAtIdx ): [(i, oldValAtIdx) | i<- [idx+1..idx+matches]] where 
                  where
                    oldValAtIdx = currentAr ! i
                    matchesForIdx = matchAr ! i

allPointsAr :: [Card] -> Int
allPointsAr cards = f emptyAr $ length cards
  where
    emptyAr :: IntArray
    emptyAr = A.listArray (1, length cards) $ repeat 0
    matchAr = calcMatchAr cards
    f resAr i
      | i == 0 = 0
      | otherwise = resFori + f updatedAr (i - 1)
      where
        resFori = 1 + sum (map (resAr !) [i + 1 .. i + (matchAr ! i)])
        updatedAr = resAr // [(i, resFori)]

--points'MR = fix (memoize.pointsM)
cardsToIdxCards :: [Card] -> [IdxCard]
cardsToIdxCards cards =
  zipWith (\cardId card -> IdxCard cardId card) [1 ..] cards

-- allPointsC ::  [Card] -> Int
-- allPointsC  cards = calcImpl curRes emptyArr   1 + sum  (map (points cards) copiedCards) where
--     copiedCards = take (matches card) $ drop idx cards
--type MapIntInt 
--matchMap 
-- pointMap = Data.map Id Matches
-- countMap = Data.map Id Counts
-- solution2 = if countMap neprazdna then totalPoints pointMap countMap + solution2 pointMap updatedCountMap else 0
type IntArray = A.Array Int Int

calcMatchAr :: [Card] -> IntArray
calcMatchAr cards = A.listArray (1, length cards) $ matches <$> cards

solution1 :: String -> Int
solution1 file = sum $ (pointsOnCard <$> cards)
  where
    cards = fileToCards file

solution2 :: String -> Int
solution2 file = allPointsAr cards
  where
    cards = fileToCards file

getSolutions4 :: String -> IO (Int, Int)
getSolutions4 filename = do
  file <- readFile filename
  print $ fileToCards file
  let s1 = solution1 file
  let s2 = solution2 file
  return (s1, s2)
