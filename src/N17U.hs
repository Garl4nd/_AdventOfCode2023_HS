{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE InstanceSigs #-}

module N17U
    (getSolutions17U) where

import  qualified Data.Array.Unboxed as A
import Data.Array.Unboxed ((!), (//))
import qualified Data.Heap as H
import qualified Data.Set as S
import GHC.List (foldl')
import Control.Monad.ST 

import Data.Array.ST (runSTArray, runSTUArray, newArray, writeArray, readArray, thaw, STArray, STUArray)--, unsafeThaw)
import Debug.Trace 
import Control.Monad (mapM_, forM_, forM, when, guard)
import Data.Char (digitToInt)
import N1 (getSolutions1)
import Data.List (sortOn)
import Data.Array.Base (writeArray, unsafeThaw, unsafeThawSTUArray)

type Pos2D = (Int, Int)
data Dir = L | U | D | R deriving (Show, Eq, Ord, A.Ix)
type AugPos = (Pos2D, Dir, Int) 
type NumType = Int
type Edges node  = [(node, NumType)]
type LabeledGraph node = A.Array node (Edges node)
type DistanceMap node = A.UArray node Distance 
type DistanceMapST node s = STUArray s node Distance 

--data Distance =  Dist NumType | Inf deriving (Eq, Show)
type Distance = NumType
inf :: Int = 100000000
class LabeledGraphC graph node valType where
    getEdges ::graph -> node -> [(node, valType)]

instance A.Ix node => LabeledGraphC (A.Array node (Edges node)) node NumType where
    getEdges :: A.Array node (Edges node) -> node -> [(node, NumType)]
    getEdges graphAr node = graphAr ! node


maxMoves1 :: Int
maxMoves1 = 3 

maxMoves2 :: Int
maxMoves2 = 10 

minMoves2 :: Int
minMoves2 = 4

traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x =  trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x
traceWInfo2 :: Show a => [Char] -> a -> b -> b
traceWInfo2 infoStr x passThrough = trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) passThrough





data DijkstraState node = DijkstraState    {
    finalStates :: S.Set node, 
    distanceMap :: DistanceMap node,
    nodeQueue :: H.MinPrioHeap Distance node,
    dests :: S.Set node
    --path :: [(node, NumType)]
} deriving Show

data DijkstraStateST node s = DijkstraStateST    {
    finalStatesST :: S.Set node, 
    distanceMapST :: DistanceMapST node s,
    nodeQueueST :: H.MinPrioHeap Distance node,
    destsST :: S.Set node
    --path :: [(node, NumType)]
} --deriving Show
--class Container a i b where
 --   get :: a -> i ->  b
--    update :: a -> e -> a

opDir :: Dir -> Dir
opDir U = D
opDir D = U
opDir R = L
opDir L = R

allDirs :: [Dir]
allDirs = [L, U, D, R]


updateDS :: forall node graph. (A.Ix node, Show node, LabeledGraphC graph node NumType) => graph -> DijkstraState node ->  DijkstraState node
updateDS _ ds
    | S.null $ dests ds = ds 
updateDS graph ds@DijkstraState {finalStates = fs, distanceMap = dm, nodeQueue = nq, dests = dsts} = case H.view nq of 
    Nothing ->  ds
    Just (distNode, nq')  ->   processNode distNode where
        processNode :: (Distance, node) ->  DijkstraState node
        processNode (_, minNode) 
            |  S.member minNode fs =  updateDS graph (ds {nodeQueue = nq'}  ) 
            -- |  S.member minNode  $ dests ds =  ds {finalStates = S.insert minNode fs, nodeQueue = nq'}
        processNode (dist, minNode) = updateDS graph $ ds {finalStates = S.insert  minNode fs, distanceMap = updatedDistanceMap, nodeQueue = updatedQueue, dests = S.delete minNode dsts }  where
            updatedDistanceMap = if null updateList then dm else  --dm // updateList -- [(--updateList 
                 runSTUArray $ do
                     stAr <- unsafeThawSTUArray dm  -- // updateList 
                     forM_ updateList $ \(pos, newDist) -> writeArray stAr pos newDist
                     return stAr
            updateList = [ (neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph  minNode, S.notMember neighbor fs,  let newDist =  dist +  edgeVal, newDist <= dm ! neighbor  ] 
            updatedQueue = foldl' (\q (node, dist')    ->  H.insert (dist', node) q  ) nq' updateList         

updateDSST :: forall node graph s. (A.Ix node, Show node, LabeledGraphC graph node NumType) => graph -> ST s (DijkstraStateST node s) ->  ST s (DijkstraStateST node s)
updateDSST graph dsST = do
    ds@DijkstraStateST {finalStatesST = fs, distanceMapST = dm, nodeQueueST = nq, destsST = dsts}  <- dsST
    if S.null $ dsts  then 
        dsST -- return ds
    else
        case H.view nq of 
            Nothing ->  dsST -- return ds
            Just (distNode, nq')  ->   processNode distNode where
                processNode :: (Distance, node) ->  ST s (DijkstraStateST node s)
                processNode (_, minNode) 
                    |  S.member minNode fs =  updateDSST graph (return $ ds {nodeQueueST = nq'}  )                                 
                processNode (dist, minNode) = do                                                             
                    let candidateList = [ (neighbor, newDist) | (neighbor, edgeVal) <- getEdges graph  minNode, S.notMember neighbor fs,  let newDist =  dist +  edgeVal]
                    currentDists <- forM candidateList (\(neigh, _) -> readArray dm neigh)
                    let updateList = [ (neighbor, newDist) | ((neighbor, newDist), currentDist) <- zip candidateList currentDists, newDist <= currentDist  ] 
                        updatedQueue = foldl' (\q (node, dist')    ->  H.insert (dist', node) q  ) nq' updateList             
                    forM_ updateList $ \(pos, newDist) -> writeArray dm pos newDist                    
                    updateDSST graph  (return $ ds {finalStatesST = S.insert  minNode fs, distanceMapST = dm, nodeQueueST = updatedQueue, destsST = S.delete minNode dsts } )

runDijkstra :: (A.Ix node, Show node) => LabeledGraph node-> node -> [node] -> DijkstraState node
runDijkstra graph start ends =  traceWInfo2 "" "Running the unsafe normal version" $ updateDS graph initState where
    initState = DijkstraState {finalStates = S.empty, distanceMap = initDistanceMap, nodeQueue = H.singleton ( 0, start), dests = S.fromList ends }
    initDistanceMap = runSTUArray $ do 
        stAr <- newArray (A.bounds graph ) inf
        writeArray stAr start  $ 0
        return stAr

runDijkstraSTLow :: (A.Ix node, Show node) => LabeledGraph node-> node -> [node]  -> ST s (DijkstraStateST node s)
runDijkstraSTLow graph start ends =  (updateDSST graph initState) where
    initState = do 
        stAr <- newArray (A.bounds graph ) inf
        writeArray stAr start  $ 0
        return DijkstraStateST {finalStatesST = S.empty, distanceMapST = stAr, nodeQueueST = H.singleton (  0, start), destsST = S.fromList ends }

runDijkstraST :: (A.Ix node, Show node) => LabeledGraph node-> node -> [node]  -> DijkstraState node
runDijkstraST graph start ends = traceWInfo2 "" "Running the ST  version" $ DijkstraState {finalStates = fs, distanceMap = dm, nodeQueue = nq, dests = dst } where-- DijkstraState {distanceMap = dm}
     stResult = runDijkstraSTLow graph start ends
     dm = runSTUArray $ do  
        DijkstraStateST { distanceMapST = dmST } <- stResult -- runDijkstraST graph start ends
        return dmST
     (fs, nq, dst )= runST  $ do
        DijkstraStateST { destsST = dst, nodeQueueST = nq', finalStatesST = fs' }  <- stResult
        return (fs',  nq',dst)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f ls = head (sortOn f ls)

getPath :: LabeledGraph Pos2D -> DistanceMap Pos2D  -> Pos2D -> Pos2D-> [Pos2D]
getPath graph dm start end = go end []  where
    go current path
         | current == start = path
         | otherwise =  let neighbors =   [pos'| dir <- allDirs, let pos' = calcPos current dir, A.inRange (A.bounds dm) pos' ] 
                            nextNode = minimumOn (dm ! ) neighbors     
                        in  go nextNode $ current:path                


solveWDijkstra :: (A.Ix node, Show node) => LabeledGraph node-> node  -> [node] -> DistanceMap node
solveWDijkstra graph start ends =  distanceMap $ runDijkstra graph start ends

solveWDijkstraST :: (A.Ix node, Show node) => LabeledGraph node-> node  -> [node] -> DistanceMap node
solveWDijkstraST graph start ends = distanceMap $ runDijkstraST graph start ends

solveAugmentedGraph :: LabeledGraph AugPos -> Pos2D -> Pos2D -> Int -> Int-> Distance
solveAugmentedGraph graph start end minMoves maxMoves = let initStates = [(start, dir, 0) | dir <- [R, D] ]-- allDirs] 
                                                            endStates =  [(end, dir, moves) | dir <- [R, D], moves <- [minMoves..maxMoves]] 
                                          --in minimum $ concat [[ traceWInfo ("solution for "<>show initState <> ", "<> show finalState<>"=") $ (solveWDijkstra graph initState [finalState]) ! finalState | initState <- initStates] | finalState <- endStates ]
                                          in minimum $ concat [traceWInfo ("Solutions for "<>show initState <>"=") $ 
                                            let distMap = (solveWDijkstraST graph initState endStates) in map (distMap !) endStates | initState <- initStates]

genEdges :: Pos2D ->  (Pos2D, Pos2D) -> NumType -> Edges Pos2D
genEdges (y,x) ((ymin, xmin), (ymax, xmax)) val = [(pos', val) | dir <- allDirs, let pos' = calcPos (y,x) dir, inBounds pos'] where --[((adjY, x), val) | s<- [1,-1], let adjY = y+s, inBounds ymin adjY ymax] ++ [((y, adjX), val) | s<- [-1,1], let adjX = x+s, inBounds xmin adjX xmax] where
    inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax

genEdgesM :: Pos2D ->  (Pos2D, Pos2D) -> A.Array Pos2D NumType -> Edges Pos2D
genEdgesM (y,x) ((ymin, xmin), (ymax, xmax)) nodeAr = [(pos', nodeAr ! pos' ) | dir<- allDirs, let pos'  =  calcPos (y,x) dir, inBounds pos']                                                        
                                                       where
    inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax

calcPos :: Pos2D -> Dir -> Pos2D
calcPos (ys,xs) U =  (ys -1, xs)
calcPos (ys,xs) D =  (ys +1, xs)
calcPos (ys,xs) L =  (ys, xs-1)
calcPos (ys,xs) R =  (ys, xs+1)

calcPosWStep :: Pos2D -> Dir -> Int -> Pos2D
calcPosWStep (ys,xs) U step =  (ys -step, xs)
calcPosWStep (ys,xs) D step =  (ys +step, xs)
calcPosWStep (ys,xs) L step =  (ys, xs-step)
calcPosWStep (ys,xs) R step =  (ys, xs+step)


genAugEdges :: AugPos ->  (Pos2D, Pos2D) -> A.Array Pos2D NumType -> Edges AugPos
genAugEdges (pos, dir, moves) ((ymin, xmin), (ymax, xmax)) nodeAr =  liftedEdges where     
     liftedEdges = do 
        dir' <- filter (/= opDir dir) allDirs 
        let moves' = if dir == dir' then moves+1 else 1
            pos' = calcPos pos dir'
        guard (moves' <=maxMoves1 && inBounds pos')
        return ((pos', dir', moves'), nodeAr ! pos')                   
     inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax

genAugEdges2 :: AugPos ->  (Pos2D, Pos2D) -> A.Array Pos2D NumType -> Int -> Int-> Edges AugPos
genAugEdges2 (pos@(y, x), dir, moves) bounds@((ymin, xmin), (ymax, xmax)) nodeAr minMoves maxMoves = -- if moves < minMoves then 
    --          let pos' = calcPosWStep pos dir (minMoves - moves) in                
    --              if inBounds pos' then [((pos', dir, minMoves), nodeAr ! pos')] else []
    --  else
      liftedEdges where     
     liftedEdges = do                 
        dir' <- if moves < minMoves then [dir] else   filter (/= opDir dir) allDirs 
        --dir' <- filter (/= opDir dir) allDirs 
        let moves' = if dir == dir' then moves+1 else 1           
            pos' = calcPos pos dir'
        guard (moves' <=maxMoves &&  inBounds pos')
        return ((pos', dir', moves'), nodeAr ! pos')                   
     inBounds (y,x) = ymin <= y && y <= ymax && xmin <= x && x <= xmax




nodeMapToGraph :: A.Array Pos2D NumType -> LabeledGraph Pos2D
nodeMapToGraph nodeMap = A.listArray bounds listVals where
    bounds@((ymin, xmin), (ymax, xmax)) = A.bounds nodeMap
    coords = concat [[(y,x) |x <- [xmin..xmax]] | y <- [ymin..ymax]]
    listVals = [genEdgesM coord bounds nodeMap | coord <- coords]

nodeMapToAugmentedGraph :: A.Array Pos2D NumType -> Int -> Int -> LabeledGraph AugPos
nodeMapToAugmentedGraph nodeMap minMoves maxMoves = A.listArray augBounds listVals where
    bounds@((ymin, xmin), (ymax, xmax)) = A.bounds nodeMap
    augBounds = (((ymin, xmin), L, 0), ((ymax, xmax), R, maxMoves))
    coords = concat [[(y,x) |x <- [xmin..xmax]] | y <- [ymin..ymax]]
    dirs = [L, U, D, R]
    augCoords = do  
        coord <- coords
        dir <- dirs
        moves <- [0..maxMoves]
        return (coord, dir, moves)
    listVals = [genAugEdges2 augCoord bounds nodeMap minMoves maxMoves | augCoord <- augCoords]



distToStr :: Distance -> String
distToStr x = show x

numGridToStr ::  DistanceMap Pos2D -> [String]
numGridToStr charGrid = let rows = [[charGrid ! (y,x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                            ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid in 
                            show <$> rows   

printDistMap :: DistanceMap Pos2D -> IO () 
printDistMap distMap =  do 
    mapM_  putStrLn $ numGridToStr  distMap

strToAR :: String -> A.Array (Int, Int) Int
strToAR strAr = A.listArray ((1,1), (lineCount, colCount)) $ digitToInt <$> (concat $ lns) where
    lns = lines strAr
    lineCount = length lns
    colCount = length $ lns !! 0

solveSimple :: String -> IO (Int, Int)
solveSimple filename =  do 
    file <- readFile filename
    let ar = strToAR file
        graph = nodeMapToGraph  ar 
        (startPos, endPos) = A.bounds graph
        ds = runDijkstra graph startPos [endPos]    
    printDistMap  (distanceMap ds)
    print "Path = "
    print $ getPath graph (distanceMap ds) startPos endPos
    return ( (distanceMap ds ) ! endPos, 0)
    --print $ solveWDijkstra graph (startPos, R, 3) (endPos, R, 2)

getSolutions17U :: String -> IO (Int, Int)
getSolutions17U filename =  do 
    file <- readFile filename
    let ar = strToAR file
        graph = nodeMapToAugmentedGraph  ar 1 maxMoves1
        graph2 = nodeMapToAugmentedGraph  ar minMoves2 maxMoves2

        ((startPos, _, _), (endPos, _, _)) = A.bounds graph
        solution1 =  solveAugmentedGraph graph startPos endPos 1 maxMoves1          
        solution2 =   solveAugmentedGraph graph2 startPos endPos minMoves2 maxMoves2
    return (  solution1,  solution2)



