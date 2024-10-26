{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Rank2Types #-}


--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE InstanceSigs #-}

module N17
    (getSolutions17) where

import  qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!), (//))
import qualified Data.Heap as H
import qualified Data.Set as S
import GHC.List (foldl')
import Control.Monad.ST 

import Data.Array.ST (runSTArray, newArray, writeArray, readArray, thaw, STArray)--, unsafeThaw)
import Debug.Trace 
import Control.Monad (mapM_, forM_, forM, when, guard)
import Data.Char (digitToInt)
import N1 (getSolutions1)
import Data.List (sortOn, unfoldr)
import Data.Array.Base (writeArray, unsafeThaw)
import GHC.Arr (unsafeThawSTArray, unsafeFreezeSTArray)
type Pos2D = (Int, Int)
data Dir = L | U | D | R deriving (Show, Eq, Ord, A.Ix)
data RedDir = H | V deriving (Show, Eq, Ord, A.Ix)

type AugPos = (Pos2D, RedDir) 
type NumType = Int
type Edges node  = [(node, NumType)]
type ArrayGraph node = A.Array node (Edges node)
type DistanceMap node = A.Array node Distance 
type DistanceMapST node s = STArray s node Distance 

data Distance =  Dist NumType | Inf deriving (Eq, Show)
--type Distance = NumType

class A.Ix node =>  LabeledGraph graph node  where
    getEdges ::graph -> node -> [(node, NumType)]
    getBounds ::  graph ->  (node, node)

instance A.Ix node => LabeledGraph (A.Array node (Edges node)) node  where
    getEdges :: A.Array node (Edges node) -> node -> [(node, NumType)]
    getEdges  = (!) --graphAr ! node
    getBounds = A.bounds 
    
maxMoves1 :: Int
maxMoves1 = 3 

maxMoves2 :: Int
maxMoves2 = 10 

minMoves1 :: Int
minMoves1 = 0

minMoves2 :: Int
minMoves2 = 3

traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x =  trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x
traceWInfo2 :: Show a => [Char] -> a -> b -> b
traceWInfo2 infoStr x passThrough = trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) passThrough

instance Ord Distance where    
    (<=) :: Distance -> Distance -> Bool
    Inf <= Inf = True
    Inf <= Dist _ = False
    Dist _ <= Inf = True
    Dist x <= Dist y = x <=y

addDist :: Distance -> NumType -> Distance
addDist (Dist x) y = Dist (x+y)
addDist _ _ = Inf

data DijkstraState node = DijkstraState    {
    finalStates :: S.Set node, 
    distanceMap :: DistanceMap node,
    nodeQueue :: H.MinPrioHeap Distance node,
    dests :: S.Set node
} deriving Show

data DijkstraStateST node s = DijkstraStateST    {
    finalStatesST :: S.Set node, 
    distanceMapST :: DistanceMapST node s,
    nodeQueueST :: H.MinPrioHeap Distance node,
    destsST :: S.Set node
} 

dijkstraLoop :: forall node graph s. (Show node, LabeledGraph graph node) => graph -> ST s (DijkstraStateST node s) ->  ST s (DijkstraStateST node s)
dijkstraLoop graph dsST = do
    ds@DijkstraStateST {finalStatesST = fs, distanceMapST = dm, nodeQueueST = nq, destsST = dsts}  <- dsST    
    if S.null $ dsts  then 
        dsST -- return ds
    else
        case H.view nq of 
            Nothing ->  dsST -- return ds
            Just (distNode, nq')  ->   processNode distNode where
                processNode :: (Distance, node) ->  ST s (DijkstraStateST node s)
                processNode (_, minNode) 
                    |  S.member minNode fs =  dijkstraLoop graph (return $ ds {nodeQueueST = nq'}  )                                 
                processNode (dist, minNode) = do                                                             
                    let candidateList = [ (neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph  minNode, S.notMember neighbor fs,  let newDist = addDist dist  edgeVal]
                    currentDists <- forM candidateList (\(neigh, _) -> readArray dm neigh)
                    let updateList = [ (neighbor, newDist) | ((neighbor, newDist), currentDist) <- zip candidateList currentDists, newDist <= currentDist  ] 
                        updatedQueue = foldl' (\q (node, dist')    ->  H.insert (dist', node) q  ) nq' updateList             
                    forM_ updateList $ \(pos, newDist) -> writeArray dm pos newDist                    
                    dijkstraLoop graph  (return $ ds {finalStatesST = S.insert  minNode fs, distanceMapST = dm, nodeQueueST = updatedQueue, destsST = S.delete minNode dsts } )

aStarLoop :: forall node graph s. (Show node, LabeledGraph graph node) => graph -> ST s (DijkstraStateST node s) ->  ST s (DijkstraStateST node s)
aStarLoop graph dsST = do
    ds@DijkstraStateST {finalStatesST = fs, distanceMapST = dm, nodeQueueST = nq, destsST = dsts}  <- dsST    
    if S.null $ dsts  then 
        dsST -- return ds
    else
        case H.view nq of 
            Nothing ->  dsST -- return ds
            Just (distNode, nq')  ->   processNode distNode where
                processNode :: (Distance, node) ->  ST s (DijkstraStateST node s)
                processNode (_, minNode) 
                    |  S.member minNode fs =  dijkstraLoop graph (return $ ds {nodeQueueST = nq'}  )                                 
                processNode (dist, minNode) = do                                                             
                    let candidateList = [ (neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph  minNode, S.notMember neighbor fs,  let newDist = addDist dist  edgeVal]
                    currentDists <- forM candidateList (\(neigh, _) -> readArray dm neigh)
                    let updateList = [ (neighbor, newDist) | ((neighbor, newDist), currentDist) <- zip candidateList currentDists, newDist <= currentDist  ] 
                        updatedQueue = foldl' (\q (node, dist')    ->  H.insert (dist', node) q  ) nq' updateList             
                    forM_ updateList $ \(pos, newDist) -> writeArray dm pos newDist                    
                    dijkstraLoop graph  (return $ ds {finalStatesST = S.insert  minNode fs, distanceMapST = dm, nodeQueueST = updatedQueue, destsST = S.delete minNode dsts } )

runDijkstraST ::  forall graph node. (Show node, LabeledGraph graph node) => graph -> node -> [node]  -> DijkstraState node
runDijkstraST graph start ends =  extractFromST $ dijkstraLoop graph initState  where
    extractFromST :: (forall s. ST s (DijkstraStateST node s)) -> DijkstraState node
    extractFromST stRes = DijkstraState {finalStates = fs, distanceMap = dm, nodeQueue = nq, dests = dst } where
        dm = runSTArray $ do  
            DijkstraStateST { distanceMapST = dmST } <- stRes -- runDijkstraST graph start ends
            return dmST
        (fs, nq, dst )= runST  $ do
            DijkstraStateST { destsST = dst, nodeQueueST = nq', finalStatesST = fs' }  <- stRes
            return (fs',  nq',dst)            
    initState ::  ST s (DijkstraStateST node s)
    initState =  do 
                            stAr <- newArray (getBounds graph ) Inf
                            writeArray stAr start  $ Dist 0
                            return DijkstraStateST {finalStatesST = S.empty, distanceMapST = stAr, nodeQueueST = H.singleton ( Dist 0, start), destsST = S.fromList ends } -- runDijkstraSTLow graph start ends

solveWDijkstraST :: (Show node, LabeledGraph graph node) => graph -> node  -> [node] -> DistanceMap node
solveWDijkstraST graph start ends = distanceMap $ runDijkstraST graph start ends

solveAugmentedGraph :: ArrayGraph AugPos -> Pos2D -> Pos2D ->  Distance
solveAugmentedGraph graph start end = let initStates = [(start, dir) | dir <- [H, V] ]
                                          endStates =  [(end, dir) | dir <- [H, V]]                                  
                                          in minimum $ concat [traceWInfo ("Solutions for "<>show initState <>"=") $ 
                                          let distMap =  solveWDijkstraST graph initState endStates in map (distMap !) endStates | initState <- initStates]

calcPosWStep :: Pos2D -> RedDir -> Int -> Pos2D
calcPosWStep (ys,xs) V step =  (ys +step, xs)
calcPosWStep (ys,xs) H step =  (ys, xs + step)

genAugEdges :: AugPos ->  (Pos2D, Pos2D) -> A.Array Pos2D NumType -> Int -> Int-> Edges AugPos
genAugEdges (pos, rdir) ((ymin, xmin), (ymax, xmax)) nodeAr minSteps maxSteps =   liftedEdges1 ++ liftedEdges2 where     
     rdir' = if rdir == V then H else V     
     liftedEdges1 = drop minSteps $ unfoldr (genFunct 1) ( pos, 0, 1)
     liftedEdges2 = drop minSteps $ unfoldr (genFunct (-1)) (pos , 0, 1)

     genFunct sign (curentPos, cost, steps) 
      | steps > maxSteps = Nothing      
      |otherwise = let newPos = calcPosWStep curentPos rdir' sign
                       newCost = cost + nodeAr ! newPos in
                        if inBounds newPos then  Just ( ( (newPos, rdir'), newCost), (newPos, newCost, steps+1)) 
                        else Nothing     
     inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax

nodeMapToAugmentedGraph :: A.Array Pos2D NumType -> Int -> Int -> ArrayGraph AugPos
nodeMapToAugmentedGraph nodeMap minMoves maxMoves = A.listArray augBounds listVals where
    bounds@((ymin, xmin), (ymax, xmax)) = A.bounds nodeMap
    augBounds = (((ymin, xmin), H), ((ymax, xmax), V))
    coords = concat [[(y,x) |x <- [xmin..xmax]] | y <- [ymin..ymax]]
    dirs = [H,V]
    augCoords = do  
        coord <- coords
        dir <- dirs
        return (coord, dir)
    listVals = [  genAugEdges augCoord bounds nodeMap minMoves maxMoves | augCoord <- augCoords]

--allDirs = [L, R, U, D]
genEdgesM :: Pos2D ->  (Pos2D, Pos2D) -> A.Array Pos2D NumType -> Edges Pos2D
genEdgesM (y,x) ((ymin, xmin), (ymax, xmax)) nodeAr = [(pos', nodeAr ! pos' ) | dir<- [H,V], let pos'  =  calcPosWStep (y,x) dir 1, inBounds pos']                                                        
                                                       where
    inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax
nodeMapToGraph :: A.Array Pos2D NumType -> ArrayGraph Pos2D
nodeMapToGraph nodeMap = A.listArray bounds listVals where
    bounds@((ymin, xmin), (ymax, xmax)) = A.bounds nodeMap
    coords = concat [[(y,x) |x <- [xmin..xmax]] | y <- [ymin..ymax]]
    listVals = [genEdgesM coord bounds nodeMap | coord <- coords]
strToAR :: String -> A.Array (Int, Int) Int
strToAR strAr = A.listArray ((1,1), (lineCount, colCount)) $ digitToInt <$> (concat $ lns) where
    lns = lines strAr
    lineCount = length lns
    colCount = length $ head lns 

fromDist :: Distance -> Int
fromDist ( Dist x) =  x
fromDist Inf=  100000

getSolutions17 :: String -> IO (Int, Int)
getSolutions17 filename =  do 
    file <- readFile filename
    let ar = strToAR file
        graph = nodeMapToAugmentedGraph  ar minMoves1 maxMoves1
        graph2 = nodeMapToAugmentedGraph  ar minMoves2 maxMoves2
        ((startPos, _), (endPos, _)) = A.bounds graph
        solution1 =  fromDist $  solveAugmentedGraph graph startPos endPos 
        solution2 =  fromDist $ solveAugmentedGraph graph2 startPos endPos 
    return ( solution1,   solution2)
    



