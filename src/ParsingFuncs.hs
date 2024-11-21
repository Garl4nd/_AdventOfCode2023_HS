module ParsingFuncs
  ( wordsWhen
  , splitOn
  , readStrList
  , splitBySubstr
  , writeXY
  , pairs
  , trimSpace
  , trimChar
  , countIf
  , groupBySorted
  , groupByUnique
  ) where

import Data.Char (isNumber)
import Data.List (findIndex, intercalate, isPrefixOf, tails, groupBy, sortOn)
import Data.Function (on)

wordsWhen :: (a -> Bool) -> [a] -> [[a]]
wordsWhen p s =
  case dropWhile p s of
    [] -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = wordsWhen (== c)

trimIf :: (a -> Bool) -> [a] -> [a]
trimIf p = reverse . (dropWhile p) . reverse . (dropWhile p)

trimChar :: Eq a => a -> [a] -> [a]
trimChar c = trimIf (== c)

trimSpace :: String -> String
trimSpace = trimIf (== ' ')

readStrList :: (Read a, Num a) => Char -> String -> [a]
readStrList delim str = read <$> splitOn delim str

findSublist :: Eq a => [a] -> [a] -> Maybe Int
findSublist subList ls = findIndex (isPrefixOf subList) (tails ls)

splitBySubstr :: Eq a => [a] -> [a] -> [[a]]
splitBySubstr delim str =
  case findSublist delim str of
    Nothing -> [str]
    Just idx ->
      take idx str : splitBySubstr delim (drop (idx + length delim) str)

pairs :: [a] -> Maybe [(a, a)]
pairs (x0:x1:xs) =  ((x0, x1) :) <$> pairs xs
pairs [_] = Nothing
pairs [] = Just []

writeXY :: Show a => String -> [a] -> [a] -> IO ()
writeXY fileName xs ys = do
  writeFile fileName
    $ intercalate "\n"
    $ zipWith (\x y -> show x <> ", " <> show y) xs ys

groupByUnique :: (Ord b) => (a->b) -> [a] -> [(a, Int)]
groupByUnique ordFunc ls = map (\grp -> (fst $ head grp, length grp)) (  groupBy ( (==) `on` snd) sortedList) where
   sortedList = sortOn snd $ map (\x -> (x, ordFunc x)) ls

--groupBySorted :: (Ord b) => (a->b) -> [a] -> [([a],b)]
groupBySorted ordFunc ls =  map (\grp -> (fst <$> grp, snd.head $ grp)) $ groupBy ( (==) `on` snd) sortedList where
   sortedList = sortOn snd $ zip ls mappedList
   mappedList = ordFunc <$> ls


countIf :: (a-> Bool) -> [a] -> Int
countIf p ls = length $ filter p ls
