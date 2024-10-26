{-# LANGUAGE CPP #-}
{-# BANG-PATTERNS #-}
{-# FlexibleContexts #-}
-- #define DEBUG
module N14
  ( getSolutions14
  ) where
import GHC.Conc (numCapabilities)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as AU
import Data.Array.Unboxed ((!), (//))
import ParsingFuncs (countIf)
import Data.List (transpose, intercalate, find)
import Data.Maybe (fromJust)
import Control.Parallel.Strategies (parMap, rpar, rdeepseq, rseq, parListChunk, dot, withStrategy)
--import Data.Func ((@))
import Debug.Trace

type CharGrid = AU.UArray (Int, Int) Char
type SplitArray = A.Array Int [[Int]]
#ifdef DEBUG
traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x = trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x
#else

traceWInfo :: p1 -> p2 -> p2
traceWInfo infoStr x = x
#endif

data SplitArs = SplitArs {uSplits :: SplitArray, dSplits :: SplitArray, lSplits :: SplitArray,rSplits :: SplitArray}


cycleDetectionFloyd' :: Eq a => [a] -> a -> (Int, Int)
cycleDetectionFloyd' fIterates x0 = (mu,lambda) where    
  nu = fromJust $ find (\i -> fIterates !! i == fIterates !! (2*i)) [1..]  
  mu = fromJust $ find (\i -> fIterates !! i == fIterates !! (i+nu)) [1..]
  lambda = fromJust $ find (\i -> fIterates !! mu == fIterates !! (mu+i)) [1..]


strToIntGrid :: String -> CharGrid
strToIntGrid str = AU.listArray ((1, 1), (numLines, lineSize)) $ concat ls  where
  ls = lines str
  numLines = length ls
  lineSize = length $ head ls
  
findAllAlongY :: Char -> CharGrid -> A.Array Int [Int]
findAllAlongY c ar = A.listArray (xmin,xmax) [ 0:[y | y <- [1..ymax], ar ! (y, x) == c ]++[ymax+1] | x<- [xmin..xmax] ] where
  ((_, xmin), (ymax, xmax)) = AU.bounds ar

findAllAlongX :: Char -> CharGrid -> A.Array Int [Int]
findAllAlongX c ar = A.listArray (ymin,ymax) [ 0:[x | x <- [1..xmax], ar ! (y, x) == c ]++[xmax+1] | y<- [ymin..ymax] ] where
  ((ymin, xmin), (ymax, xmax)) = AU.bounds ar

findAllSplits :: [Int] -> [[Int]]--A.Array Int [(Int,Int)]
findAllSplits  blockPositions = let pairs = zip blockPositions (tail blockPositions) in [[minPos+1..maxPos-1] | (minPos, maxPos) <- pairs, maxPos > minPos +1 ]-- zip (blockPositions ! x) (drop 1  (blockPositions! x))

splitArrays:: Char -> CharGrid -> SplitArs
splitArrays c ar = SplitArs upSplits (fmap (reverse <$>) upSplits) riSplits (fmap (reverse <$>) riSplits) where
  splits:: Int -> Int-> A.Array Int [Int] -> SplitArray 
  splits minInd maxInd delims = A.listArray (minInd, maxInd) [findAllSplits (delims ! x ) | x<- [minInd..maxInd]] 
  ((ymin,xmin),(ymax,xmax)) = AU.bounds ar
  upSplits  = splits xmin xmax verticalDelims
  riSplits = splits ymin ymax horizontalDelims
  verticalDelims = findAllAlongY c ar
  horizontalDelims = findAllAlongX c ar

data Dir  = V | H deriving Eq

parMapChunk n strat f = withStrategy (parListChunk n strat) . map f

move :: SplitArray -> Dir-> CharGrid -> CharGrid
move  splits dir charGrid =  charGrid // concatMap moveList  ortBounds  where
   --charGrid // (concat $! moves) where --charGrid // concatMap  moveList  ortBounds  where
   --moves = parMapChunk 25 (rpar `dot` rdeepseq)  moveList ortBounds
   ((ymin,xmin),(ymax,xmax)) = AU.bounds charGrid
   ortBounds = if dir == V then [xmin..xmax] else [ymin..ymax]
   moveList ::  Int -> [((Int, Int), Char)]
   moveList  ind = concatMap  elementaryMoves  (splits ! ind)  where
     elementaryMoves = if dir == V then elementaryMovesY  else elementaryMovesX
     elementaryMovesY :: [Int] -> [((Int, Int), Char)]
     elementaryMovesY splitRange =  [((y,ind), 'O') | y<- stoneRange] ++ [((y,ind), '.') | y <- nothingRange] where
       stoneCount = countIf (\y -> charGrid ! (y,ind) == 'O')  splitRange       
       (stoneRange, nothingRange) =  splitAt stoneCount splitRange
     elementaryMovesX ::  [Int] -> [((Int, Int), Char)]
     elementaryMovesX  splitRange =  [((ind,x), 'O') | x<- stoneRange]  ++ [((ind,x), '.') | x <- nothingRange] where
       stoneCount = countIf (\x -> charGrid ! (ind,x) == 'O')  splitRange       
       (stoneRange, nothingRange) =  splitAt stoneCount splitRange
  

loadBetweenBlocks :: CharGrid -> Int -> (Int, Int) -> Int
loadBetweenBlocks ar x (ymin, ymax) = sum [ymax - stoneCount .. ymax-1] where 
  stoneCount =  traceWInfo "count= " $ countIf (\y -> ar ! (y,x) == 'O') (traceWInfo "bounds=" [ymin+1..ymax-1])

updateStones :: CharGrid -> Int -> (Int, Int) -> CharGrid
updateStones ar x (ymin, ymax) = ar // [((y,x), 'O') | y<- [ymax - stoneCount .. ymax-1]] // [((y,x), '.') | y <- [ymin+1..ymax-stoneCount -1]] where 
  stoneCount =   countIf (\y -> ar ! (y,x) == 'O') [ymin+1..ymax-1]



cycleN :: Int -> SplitArs-> CharGrid  -> CharGrid
cycleN n splitArs = (!! n). iterate (rotationCycle splitArs)   

scores :: SplitArs-> CharGrid  -> [Int]
scores splitArs initAr = score <$>  (iterate (rotationCycle splitArs) initAr  )

rotationCycle ::   SplitArs -> CharGrid -> CharGrid
rotationCycle splitArrays ar = (moveRight.moveDown.moveLeft.moveUp) ar where
  moveLeft = move (lSplits splitArrays) H
  moveRight = move (rSplits splitArrays) H  
  moveUp = move (uSplits splitArrays) V
  moveDown = move (dSplits splitArrays) V

score :: CharGrid -> Int
score charGrid =  sum [ymax - y +1| ((y,x), val) <- AU.assocs charGrid, val =='O'] where
  ((_,_),(ymax,_) ) = AU.bounds charGrid

solution1 :: String -> Int
solution1 file = score movedAr where
  ((_,_),(ymax,_) ) = AU.bounds movedAr
  movedAr = move (vertSplits)  V ar
  ar = strToIntGrid file
  vertSplits = uSplits $ splitArrays '#' ar  

solution2 :: String -> Int
solution2 file = score $ (iterate (rotationCycle splitArs) muRes) !! r where
  muRes = fIterates !! mu
  r = traceWInfo "r=" $ (1000000000 -mu) `mod` lambda  
  (mu, lambda ) =  traceWInfo "mu, lambda = " $ cycleDetectionFloyd' fIterates ar --length $ takeWhile (/= head  scoreList) $  tail scoreList  where    
  fIterates = (iterate (rotationCycle splitArs) ar  )
  ar = strToIntGrid file
  splitArs = splitArrays '#' ar  
  

  
getSimpleSolution :: String -> Int
getSimpleSolution text = sum $ (loadPerX <$> [xmin..xmax]) where
  charGrid = strToIntGrid text
  ((ymin, xmin), (ymax, xmax)) = AU.bounds charGrid
  blockPositions =  findAllAlongY '#' charGrid  
  loadPerX x = sum $ zipWith (curry $ loadBetweenBlocks charGrid x ) augBlock (drop 1  augBlock) where
    augBlock = (blockPositions ! x)
    
moveStones :: String -> CharGrid
moveStones text = foldl updateAt charGrid  [xmin..xmax]  where
  charGrid = strToIntGrid text
  ((ymin, xmin), (ymax, xmax)) = AU.bounds charGrid
  blockPositions =  findAllAlongY '#' charGrid  
  updateAt ar x = foldl  (\ar' pair -> updateStones ar' x pair) ar $ zip (blockPositions ! x) (drop 1  (blockPositions! x))

charGridToStr :: CharGrid -> [String]
charGridToStr ar = [[ar! (y,x) | x<- [xmin..xmax] ] | y<-[ymin..ymax]]     where
  ((ymin, xmin ), (ymax, xmax)) = AU.bounds ar

getSolutions14 :: String -> IO (Int, Int)
getSolutions14 filename = do   
  putStrLn $ "number of cores: " ++ show numCapabilities
  file <- readFile filename    
  --print $ moveStones file
  let ar = strToIntGrid file
  --let moved = move $ ar
 -- putStrLn $ intercalate "\n" (lines file )
  print  "\n--\n->\n" 
  --print $ dSplits $ splitArrays '#' ar  
  --print $ uSplits $ splitArrays '#' ar  

  --putStrLn $ intercalate "\n" (charGridToStr $ move (lSplits $ splitArrays '#' ar  ) H ar)  
  
  --putStrLn $ intercalate "\n" (charGridToStr $ cycleN 10000 (splitArrays '#' ar) ar )
  return (solution1 file, solution2 file)  