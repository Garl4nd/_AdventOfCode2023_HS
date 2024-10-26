{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-- #define DEBUG
-- #define DEBUG_DETAILED

module N12
  ( getSolutions12
  ) where

-- split the string into pieces between "." : s = "#???.#.#." -> sl = ["#???", #, #"]. Solutions for first:slRest firstNum:numsRest = [all solutions for first, firstNum x solutions for sufix(rest)+slRest(first):numsRest], kde sufix se pocita od prvniho #
-- solution for noDotsStr brokenNum = if length noDotsStr < brokenNum then [] else fill in at most min (brokenNum, length - num # - 1) "#" and take all with consecutive "#" = brokenNum
-- ex: "???#" 2 : fill 2 # : "##?#"  -> solution -> ["##", ".#",...] , "?###" - not a solution "#??#" - ns "?#?#" - ns, "??##" - s. -> ["..##", ...]
-- ex: "#?#?#" 3 : min (3, 5 - 3 -1 ) = 1: "###?#", "#?###"
-- ex: "###???#?# 4: min(4, 9 -5 -1 ) = 3:  fill 3: "####?####", fill 2: "####?##?#" -> ["####", ".##.#"], "####??###" -> ["####", "..###"]
-- ex: ###???#?# 4 -> ####??#?#, ####??###
-- Generovani substitutici: 
-- s1 = subt 1 '?' ls, (finalSubstitions1, rest1) = partition isValidPref s1 -> s2 = concat [subt 1 '?' s | s <- rest], (finalSubstitutions2, rest2) = ption isValidPref s2 -> s3 =...
-- reseni: feasibleSublists = splitOn (== '.') springs; solutions (current:rest) bNums = concat [prefix ++ solutions rest remainingBnums | (prefix, remainingBnums) <- solveAtomic current bNums] 
--  
import Data.List ( elemIndex, elemIndices, find, group, inits, intercalate, partition, unfoldr, tails, nub, groupBy, sortOn)
import Data.Maybe (fromJust)
import Debug.Trace
import ParsingFuncs (readStrList, splitOn, countIf)
import Data.Function (on)
import Data.Function.Memoize (memoize, memoize2, memoFix2, memoFix3)
import Data.MemoTrie (memoFix, memo2)
import Data.Array as A ( array, (!) )
import Control.Parallel.Strategies (parMap, rpar, rdeepseq, rseq, parList)
type SpringList = [SpringState]

type SpringSplit = (SpringList, SpringList)

type SpringState = Char
--data SpringState= '.' | '#'| '?' deriving (Eq, Show)

type Memo f = f -> f


trace2 x = trace ("Debug trace:" ++ show x) x
#ifdef DEBUG
traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x = trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x
#else

traceWInfo infoStr x = x
#endif

#ifdef DEBUG_DETAILED
traceWInfoD x y = traceWInfo x y
#else   
traceWInfoD _ x = x
#endif
--traceWInfoD x y = traceWInfo x y

charToSpring :: Char -> Char
charToSpring = id
-- charToSpring '.' = '.'
-- charToSpring '#' = '#'
-- charToSpring _ = '?'
springToChar :: Char -> Char
springToChar = id
-- springToChar '.' = '.'
-- springToChar '#' = '#'
-- springToChar '?' = '?'

--replaceNchars 
parseSprings :: String -> [SpringState]
parseSprings str = charToSpring <$> str

springsToStr :: [SpringState] -> String
springsToStr springs = map springToChar springs


possibleDropPositions :: [Int] -> Int -> Int -> [Int]
possibleDropPositions _ _ 0 = []
possibleDropPositions elIndices b lSprings = [x | x <- [b..ul], x `notElem` elIndices]--springs - 1], ] [(max ( firstB+1) b).. (min lsLength (firstB+b+1))] where
  where ul = case elIndices of 
            [] -> lSprings 
            (firstB:_) -> min lSprings (firstB+b)

numOfUSolutions :: Memo (Int -> [Int] -> Int)
numOfUSolutions numOfUSolutions  lsLength bNums = case bNums of 
    [] -> 1
    [b] -> nSingle lsLength b
    b:rest -> sum [numOfUSolutions (lsLength - i-1) rest | i <- [b..lsLength]]
  where
      nSingle lsL b =  if b > lsL then 0 else lsL - b + 1

numOfUSolutionsMemo :: Int -> [Int] -> Int
numOfUSolutionsMemo = memoFix2 numOfUSolutions

numOfSolutionsSingleMemo  = memo2 numOfSolutionsSingle

numOfSolutionsSingle :: SpringList -> Int -> Int
numOfSolutionsSingle  springs b = if '#' `notElem` springs then max 0 (length springs - b +1) else   
                                 let bIndices = elemIndices '#' (traceWInfoD "springs" springs)
                                     (f, l) = (head bIndices, last  bIndices) 
                                     lb =  max 0 (l - b+1)
                                     ub =  min (length springs - b) f in 
                max 0 (ub - lb +1 )   

numOfSolutionsAtomicOpt ::  SpringList -> [Int] -> Int
numOfSolutionsAtomicOpt  springList brokenNums =  numOfSolutionsAtomicOptInnerMemo 0 springList brokenNums where         
      elIndices =  A.array  (0, length springList ) $ zip [0..length springList] (( elemIndices '#') <$> (tails springList))
      numOfSolutionsAtomicOptInnerMemo =  memoFix3 numOfSolutionsAtomicOptInner
      numOfSolutionsAtomicOptInner numOfSolutionsAtomicOptInner  offset springs bNums = if '#' `notElem` springs then numOfUSolutionsMemo (length springs) bNums else case bNums of 
        [] -> if '#' `notElem` springs then 1 else 0
        [b] | '?' `notElem` springs -> if  length springs == b then 1 else 0
        [b] -> numOfSolutionsSingleMemo springs b
        b:rest ->       if  sum bNums + (length bNums -1) > length springs then 0 else 
          let elIndicesCur = elIndices ! offset in
          sum [numOfSolutionsAtomicOptInner (offset + i+1) (drop (i+1) springs) rest | i <- possibleDropPositions elIndicesCur b  $ length springs]

atomicSolutionsWPartitions::  SpringList -> [Int] -> [Int]
atomicSolutionsWPartitions   springList bNums = trivialSolution:unfoldr genSolutions ([0] , bNums) where 
  trivialSolution = if '#' `notElem` springList then 1 else 0
  elIndices =  A.array  (0, origLength ) $ zip [0..length springList] (( elemIndices '#') <$> (tails springList)) 
  origLength =  length springList
  genSolutions:: ( [Int], [Int])-> Maybe  (Int, ([Int], [Int]))
  genSolutions (offsets, brokenCounts)  =  case brokenCounts of
    [] -> Nothing
    firstBrokenCount:rest -> Just (countIf  (null.(elIndices !)) dropLocs, (dropLocs, rest))  where
        dropLocs  = concat [ genNewDropPoss  (possibleDropPositions (elIndices ! offset) firstBrokenCount  (origLength - offset)  )  offset   |  offset  <- offsets] -- misto sufixu staci dropPozice
        genNewDropPoss dropIdxs offset = [ min origLength  (offset + i+1) | i <-  dropIdxs]        

atomicSolutionsWPartitionsM = memoize2 atomicSolutionsWPartitions
numOfSolutionsByPartitionMemo = memoFix2 numOfSolutionsByPartition


numOfSolutionsByPartition :: Memo ( [SpringList] -> [Int] -> Int)
numOfSolutionsByPartition numOfSolutionsByPartition springPartition brokenCounts = case springPartition of 
  [] -> if null brokenCounts then 1 else 0 
  [springList] -> numOfSolutionsAtomicOpt springList brokenCounts  
  current:rest -> if sum  (length <$> springPartition) < sum brokenCounts then 0 else        
    sum $ zipWith (*) solutionNumList sufixResultList   where 
        solutionNumList =  atomicSolutionsWPartitionsM current $ take (length brokenCounts - numOfBPartitions +1 ) brokenCounts    --melo by se zjistit, kolik je v dalsich seznamech krizku a nedodavat vic brokenCountu
        sufixResultList = zipWith (\solNum bNums -> if solNum ==0 then 0 else numOfSolutionsByPartition rest bNums) solutionNumList $  tails brokenCounts
        numOfBPartitions = countIf ('#' `elem`) springPartition

numOfSolutions :: SpringList -> [Int] -> Int
numOfSolutions springs =  numOfSolutionsByPartitionMemo (splitOn '.' springs)

numOfSolutionsStr :: String -> [Int] -> Int
numOfSolutionsStr springStr brokenCounts =
  traceWInfo "numOfSolutions =" $   ((`numOfSolutions` brokenCounts) . parseSprings) springStr

parseLine :: String -> (String, [Int])
parseLine line = (springStr, readStrList ',' numStr)
  where
    [springStr, numStr] = words line

parseLineExpanded :: String -> Int-> (String, [Int])
parseLineExpanded line debugNum = traceWInfo ("#"++ show debugNum ++" Solving for: ") (init $ expand (springStr++"?"), expand $ readStrList ',' numStr)
  where
    [springStr, numStr] = words line
    expand ls = concat $ replicate 5 ls

solution1 :: String -> Int
solution1 file = traceWInfo "Solution 1 = " $ sum numLineSolutions
  where
    numLineSolutions = uncurry numOfSolutionsStr <$> lineTups
    lineTups = parseLine <$> lines file

findErrors :: String -> [((String, [Int]),(Int, Int)) ]
findErrors file = filter (\(tup, (l, r)) -> l /= r)  $ zip lineTups solutionPairs 
  where
    solutionPairs = zip numLineSolutions numLineSolutions'
    numLineSolutions =  uncurry numOfSolutionsStr <$> lineTups
    numLineSolutions' =  uncurry numOfSolutionsStr <$> lineTups
    lineTups = parseLine <$> lines file

solution2 :: String -> Int
solution2 file = traceWInfo "Solution 2 = " $ sum numLineSolutions
  where
    numLineSolutions = parMap rpar (uncurry numOfSolutionsStr)  lineTups
    lineTups = zipWith parseLineExpanded  (lines file) [1..] 


getSolutions12 :: String -> IO (Int, Int)
getSolutions12 filename = do
  file <- readFile filename
  return (solution1 file, solution2 file)
