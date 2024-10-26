{-# LANGUAGE FlexibleContexts #-}
module N18
 (getSolutions18) where
import qualified Data.Array as A
import Data.Array ((!))

import Data.Array.ST
import Control.Monad (forM_)
import Debug.Trace
import Numeric (readHex)

data Direction = L | R | U |D deriving (Show, Read)
data Instruction = Inst {dir :: Direction, ln :: Int, color:: String} deriving Show
type Trench = A.Array (Int, Int) Bool 
type Pos = (Int, Int)
traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x =  trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x


makeStep :: Pos -> Direction -> Int -> Pos
makeStep (y,x) dir size = case dir of  
  D -> (y+size, x)
  U -> (y-size, x)
  L -> (y, x-size)
  R -> (y, x+size)

calcCoords :: (Int, Int) -> [Instruction] -> [[Pos]]
calcCoords sizes@(h, w) instructionList = digLoop (h `div` 2, w `div` 2) instructionList where
                                  digLoop _ [] = []
                                  digLoop cPos (Inst{dir = dir, ln = ln }:rest) =
                                            let moveList = (makeStep cPos dir) <$> [0..ln-1] 
                                                newPos = makeStep cPos dir (ln) in
                                     moveList : digLoop newPos rest

calcCoords' :: [Instruction] -> [(Pos, Int)]
calcCoords'  instructionList = go (1, 1) instructionList where
                                  go _ [] = []
                                  go cPos (Inst{dir = dir, ln = ln }:rest) =
                                            let    newPos = makeStep cPos dir (ln) in
				                                            (cPos, ln) : go newPos rest


hexToInstruction :: String -> Instruction
hexToInstruction str = let code = drop 2 $ init str 
                           len =   fst $ head  (readHex $ init code)
                           dir = case last code of 
                            '0' -> R
                            '1' -> D
                            '2' -> L
                            '3' -> U
                        in 
                          Inst dir len str

vertices :: [[Pos]] -> [Pos]
vertices moveLists = head <$> moveLists

shoelaceArea :: [Pos] -> Int
shoelaceArea pts = abs $ (sum $ zipWith (\(y1, x1) (y2, x2) -> x1 * y2 - y1* x2 ) pts (drop 1 $ cycle pts) ) `div` 2

trenchArea :: (Int, Int) -> [Instruction] -> Int
trenchArea sizes insts =  shoelaceArea $ vertices $ calcCoords sizes insts  

totalFilled :: (Int, Int) -> [Instruction] -> Int
totalFilled sizes insts = let     boundary = calcCoords sizes insts  
                                  vtxs = vertices boundary
                                  area = shoelaceArea $ vtxs
                                  bl = traceWInfo "Boundary lenght = " $ sum $ length <$> boundary 
                                  innerPts = traceWInfo "Inner points =" $ area - (bl `div` 2 ) + 1
                             in   innerPts + bl

totalFilled' ::   [Instruction] -> Int
totalFilled' insts = let          (vtxs, bls) = unzip $ calcCoords'  insts  
                                  area = shoelaceArea $ vtxs
                                  bl = sum bls
                                  innerPts = traceWInfo "Inner points =" $ area - (bl `div` 2 ) + 1
                             in   innerPts + bl


makeTrench :: (Int, Int) -> [Instruction] -> Trench
makeTrench sizes@(h,w) instructions = go $ calcCoords sizes instructions   where 
  go ::  [[Pos]] -> Trench 
  go moveLists  = runSTArray $ do
                              stAr <- newArray ((1,1), sizes) False
                              let digLoop  [] = return ()
                                  digLoop  (moveList : rest) = 
                                    (forM_ moveList $ \pos -> writeArray stAr pos True) >> digLoop rest
                              digLoop  moveLists
                              return stAr

parseFile :: String -> ([Instruction])
parseFile file = let lns = lines file
                     strs = words <$> lns 
                     in map (\[dirStr, lnStr, color] -> Inst (read dirStr) (read lnStr) color ) strs

charGridToStr :: A.Array (Int, Int) Char -> [String]
charGridToStr charGrid = let rows = [[charGrid ! (y, x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                             ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid in show <$> rows   

boolGridToStr :: A.Array (Int, Int) Bool -> [String]
boolGridToStr boolGrid =  charGridToStr $ (\el -> if el then '#' else '.') <$> boolGrid

countInnerList :: [Bool] -> Int
countInnerList bl = go bl False False where
	go :: [Bool] -> Bool -> Bool -> Int
	go [] _ _ = 0
	go (cur:rest) inside lst = let     
                                  inside' = if lst && not cur then not inside else inside 
                                  inc = if inside' || cur then 1 else 0 in
                                    inc + go rest inside' cur
countInnerAr :: Trench -> Int -> Int
countInnerAr trench y = go 2 False where
	go :: Int -> Bool ->  Int
	go x _  | x == xmax = 0
	go x inside  = let inside' = if trench ! (y-1, x-1) && trench ! (y,x-1) && not cur then not inside else inside 
                           inc = if inside' || cur   then 1 else 0   
			   cur = trench ! (y,x) 
		 	   in inc + go (x+1) inside'   
	((ymin, xmin), (ymax, xmax)) = A.bounds trench


solution1 :: String -> Int
solution1 filename = 
  let instructionList = parseFile filename
  --print $ "Inner points = " <> (show $ totalFilled bounds instructionList)
  --putStr$ unlines $  boolGridToStr trench
    in totalFilled' instructionList 

solution2 :: String -> Int
solution2 filename = 
  let instructionList = parseFile filename
  --print $ "Inner points = " <> (show $ totalFilled bounds instructionList)
  --putStr$ unlines $  boolGridToStr trench
    in totalFilled' (hexToInstruction.color <$> instructionList) 

 

printTrench :: String -> IO ()
printTrench filename = do 
  file <- readFile filename
  let instructionList = parseFile file
      bounds = (2000, 2000)
      trench = makeTrench bounds instructionList
  putStr$ unlines $  boolGridToStr trench

getSolutions18 :: String -> IO (Int, Int)
getSolutions18  filename = do 
  file <- readFile filename
  return (solution1 file, solution2 file) 
