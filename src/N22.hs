module N22 
  (getSolutions22) where
import ParsingFuncs (groupBySorted, splitOn)
import Data.List (sortOn)
import qualified Data.Set as S 
import qualified Data.Map as M
import Data.Map ((!))
type Interval = (Int, Int)
type Label = Int
type BrickMap = M.Map Int Brick
data Brick = Brick {label:: Label, xI :: Interval, yI :: Interval, zI :: Interval, 
  bricksBelow :: [Label], bricksAbove :: [Label] } deriving (Show, Eq, Ord)
lowZ :: Brick -> Int
lowZ Brick {zI = (z0, _)} = z0 
highZ :: Brick -> Int
highZ  Brick {zI = (_, z1)} = z1

putBrickToZ :: Brick -> Int -> Brick
putBrickToZ brick@Brick{zI = (z0, z1)} z = brick {zI = (z, z + (z1 - z0))} 

finalArrangement :: [Brick] -> BrickMap 
finalArrangement bricks = foldl settleBrick M.empty $ sortOn lowZ bricks where
  settleBrick :: BrickMap -> Brick -> BrickMap
  settleBrick settledBricks currentBrick@Brick {label = cLabel} = 
   let 
    supportCandidates = M.elems $ M.filter (`isBelow` currentBrick) settledBricks
   in 
    if null supportCandidates then M.insert cLabel 
        (putBrickToZ currentBrick 1){bricksBelow = [], bricksAbove = []}  settledBricks
    else let 
      (supportBricks, supportHeight):_ = reverse $ groupBySorted highZ supportCandidates 
      newlySettledBrick = (putBrickToZ currentBrick $ supportHeight + 1){ 
      bricksBelow = label <$> supportBricks, bricksAbove = []}
      update lowerBrick = 
       if label lowerBrick `elem` bricksBelow newlySettledBrick then  
        lowerBrick{bricksAbove = label currentBrick:bricksAbove lowerBrick}
       else lowerBrick
    in M.insert cLabel newlySettledBrick (update <$> settledBricks) 
  isBelow lowerBrick upperBrick = overlaps (xI lowerBrick)  (xI upperBrick) && 
                                       overlaps (yI lowerBrick)  (yI upperBrick) 
   
overlaps :: Interval -> Interval -> Bool
overlaps (a1, b1) (a2, b2) = a1 <= a2 && a2 <= b1 || a2 <= a1 && a1 <= b2 

parseSnapshot :: String -> [Brick]
parseSnapshot file =  zipWith parseBrickLine [1..] (lines file) where
  parseBrickLine num line = let 
    [lb, ub] = splitOn ',' <$> splitOn '~' line 
    ([xl, yl, zl], [xu, yu, zu]) = (read <$> lb, read <$> ub)
    in Brick{xI =  (xl, xu), yI = (yl, yu), zI = (zl, zu), label = num }  

parseFile :: String -> IO [Brick]
parseFile filename = do 
   file <- readFile filename
   return $ parseSnapshot file

disintegrations :: BrickMap -> Label -> S.Set Label
disintegrations brickMap disLabel =  propagateDis (S.singleton disLabel) 
                                    (S.fromList $ (brickMap !) <$> bricksAbove currentBrick) (highZ currentBrick + 1) where 
  currentBrick = brickMap ! disLabel 
  propagateDis :: S.Set Label -> S.Set Brick -> Int -> S.Set Label
  propagateDis failedBricks bricksAtRisk _ 
     | S.null bricksAtRisk = failedBricks
  propagateDis failedBricks allBricksAtRisk z = let 
    currentBricksAtRisk = S.filter ((z ==).lowZ) allBricksAtRisk
    newlyFailedBricks = S.filter (all (`S.member` failedBricks).bricksBelow) currentBricksAtRisk
    newBricksAtRisk = S.union 
                      (S.fromList $ concatMap (map (brickMap !).bricksAbove)  $ S.toList newlyFailedBricks)
                      $ S.difference allBricksAtRisk  currentBricksAtRisk

     in propagateDis (S.union (S.map label newlyFailedBricks) failedBricks) 
                     newBricksAtRisk (z+1)   
  


solution1 :: String ->  Int
solution1 file = length bricks - S.size singleSupports where
  singleSupports = foldl (\acc Brick{bricksBelow = supports} -> 
                          if length supports == 1 then S.insert (head supports) acc else acc  ) 
                          S.empty settledBricks
  settledBricks = finalArrangement bricks 
  bricks = parseSnapshot file

solution2 :: String -> Int
solution2 file = sum $ (\chain -> length chain - 1) <$> disintChains where
  disintChains = disintegrations brickMap <$> M.keys brickMap 
  bricks = parseSnapshot file
  brickMap = finalArrangement bricks

getSolutions22 :: String -> IO (Int, Int)
getSolutions22 filename = do 
  file <- readFile filename 
  return (solution1 file, solution2 file)
