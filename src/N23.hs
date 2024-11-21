{-# Language TupleSections #-}
{-# Language BangPatterns #-}
module N23 
    (getSolutions23) where

-- omezeni na branche: Pokud do jedne krizovatky vede vic cest, tak vsechny krome nejdelsi  lze ustrihnout
-- vyrobit nejdriv oznaceny graf (krizovatky, cesty ke krizovatkam [delka cesty])

import qualified Data.Array as A
import Data.Array ((!), (//))
import qualified Data.Map as M
import Debugging (traceWInfo)
import Data.GraphViz.Types.Generalised   as G
import Data.GraphViz.Attributes.Complete
--import Data.GraphViz
import Data.GraphViz.Printing ( renderDot )
import Control.Monad.Trans.State.Lazy as St
import GraphMaker (writeLabeledGraph, writeLabeledUndirectedGraph)
import ParsingFuncs (splitBySubstr)
import Data.Maybe (fromMaybe)
import Data.List (sortOn)
type Position = (Int, Int)
type CharGrid = A.Array Position Char
type WeightedEdge = (Position, Int)
type PathGraph = M.Map Position [WeightedEdge]
data PosDis = PosDis {position:: Position, dist:: Int}

neighbors :: Position -> [Position]
neighbors (y,x) = [(y+1, x), (y-1, x), (y, x-1), (y, x+1)]

slides :: [Char]
slides = ['>', '<', '^', 'v']
slide :: Position -> Char -> Position
slide (y, x) '>' = (y, x+1)
slide (y, x) '<' = (y, x-1)
slide (y, x) '^' = (y-1, x)
slide (y, x) 'v' = (y+1, x)

makePathGraph :: CharGrid -> Bool -> PathGraph
makePathGraph grid slideOnSlides = M.insert endPos [] $ M.unions   $ findAllWalkableJunctions <$> startPos:junctions where 
        startPos = (1,2)
        endPos = let (_, (ymax, xmax)) = A.bounds grid in (ymax, xmax -1)
        junctions = filter (\pos -> length  (walkablePlaces pos) >=3 && grid ! pos /= '#') $  A.range $ A.bounds grid         
        findAllWalkableJunctions departure = edgesForJunction  [PosDis {position = branchPos, dist = 1} | branchPos <- walkablePlaces departure] where
          edgesForJunction currentPaths = M.singleton departure $ makeStep currentPaths [departure] []
          makeStep :: [PosDis] -> [Position] -> [WeightedEdge] -> [WeightedEdge]
          makeStep [] _ edges  = edges 
          makeStep (PosDis{position = cPos, dist = cDist}:remainingPaths) visitedPlaces edges 
            | cPos `elem` endPos:junctions =  makeStep remainingPaths visitedPlaces $ (cPos, cDist):edges
          makeStep (currentPath@PosDis{position = cPos, dist = cDist}:remainingPaths) visitedPlaces edges =  case filter (`notElem` visitedPlaces) $ walkablePlaces cPos of 
            [] ->makeStep remainingPaths (cPos:visitedPlaces) edges 
            [nextPosition] -> makeStep (currentPath{position = nextPosition, dist = cDist+1}:remainingPaths) (cPos:visitedPlaces) edges 
            _ -> error $ "Invalid graph, cPos= " <> show cPos <> ", Visited places = "<>show visitedPlaces
          
        walkablePlaces pos 
          | slideOnSlides && grid ! pos `elem` slides = [slide pos $ grid ! pos]
        walkablePlaces pos = [pos' | pos'<- neighbors pos, A.inRange (A.bounds grid) pos' && grid ! pos' /= '#']     
    

strToCharGrid :: String -> CharGrid
strToCharGrid file = A.listArray ((1,1), (numLines, lineSize)) $ concat ls  where
  ls = lines file
  numLines = length ls
  lineSize = length $ head ls

highlightJunctions :: CharGrid -> CharGrid
highlightJunctions charGrid = let 
 junctions = M.keys $ makePathGraph charGrid False
 in charGrid // ((, 'J') <$> junctions) 

charGridToStr :: CharGrid -> [String]
charGridToStr charGrid = let rows = [[charGrid ! (y, x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                             ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid in show <$> rows   

charGridToStrAug :: CharGrid -> [String]
charGridToStrAug charGrid = let rows = [[charGrid ! (y, x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                                ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid 
                                numStrX x = if x< 10 then show x else show (x `mod` 10)
                                numStrY y = if y< 10 then "0"<>show y else show y
                            in ("   " <> (concat $ numStrX <$> [xmin..xmax] )): [ numStrY y <> show row | (y,row) <- zip [1..] rows]

reverseGraph :: PathGraph -> PathGraph
reverseGraph graph = M.foldrWithKey (\pos edges accMap ->  foldl (\accMap' (tg, dist) -> M.insertWith (++) tg [(pos, dist)] accMap'  ) accMap edges) (M.fromList $ (, []) <$> M.keys graph) graph

trimGraph :: [Position] -> PathGraph ->  PathGraph
trimGraph exludedPositions graph  = let 
    singlePathNodes = M.keys $ M.filterWithKey (\k v -> length v == 1 && k `notElem` exludedPositions) graph
    removeNode cMap removedName = let 
                               sourceEdges = fromMaybe [] (M.lookup  removedName $ reverseGraph cMap ) --reversedGraph) 
                               [(targetNode, distFromRemoved)] = cMap M.!  removedName
                               rewriteEdge = map (\node@(name, distToRemoved) -> if removedName == name then (targetNode, distToRemoved + distFromRemoved) else node) 
                           in  M.delete removedName $ foldl (\tMap node ->  M.adjust rewriteEdge (fst node) tMap) cMap sourceEdges
    in  foldl removeNode graph singlePathNodes 

reverseTrimGraph :: [Position] -> PathGraph ->PathGraph 
reverseTrimGraph exludedPositions = reverseGraph.trimGraph exludedPositions.reverseGraph

makeGraph :: String -> String -> Bool -> IO () -- PathGraph
makeGraph filename dotName slideOnSlides = do 
  file <- readFile filename
  let grid = strToCharGrid file
  putStr $ unlines $ charGridToStr $ grid
  print $ "________________________"
  print $ "________________________"
  putStr $ unlines $ charGridToStrAug $ highlightJunctions grid
  let pathGraph = makePathGraph grid slideOnSlides
      [fp, _] = splitBySubstr ".dot" dotName
      reversedDotName = fp <> "rev.dot"
      trimmedDotName = fp <> "trimmed.dot"
      reverseTrimmedDotName = fp <> "trimmedRev.dot"
      fullyTrimmedDotName = fp <> "fullyTrimmedRev.dot"

      endPos = let (_, (ymax, xmax)) = A.bounds grid in (ymax, xmax -1)
      excludedPositions = [(1,2), endPos]
      fullyTrimmedGraph = (reverseTrimGraph excludedPositions. trimGraph excludedPositions)  pathGraph 
  writeLabeledGraph  dotName pathGraph
  writeLabeledGraph reversedDotName $ reverseGraph pathGraph
  writeLabeledGraph trimmedDotName $ trimGraph excludedPositions pathGraph 
  writeLabeledGraph reverseTrimmedDotName $ reverseTrimGraph excludedPositions pathGraph 
  writeLabeledUndirectedGraph fullyTrimmedDotName fullyTrimmedGraph
  return () -- pathGraph
data Path = Path {visited :: [Position], pDist :: Int} deriving Show
type MaxGraph = M.Map Position Int

distUpperBound :: MaxGraph -> [Position] -> Int
distUpperBound maxGraph remainingPositions = sum $ (maxGraph  M.!) <$> remainingPositions   

makeMaxGraph :: PathGraph -> MaxGraph
makeMaxGraph graph = M.map maxEdgeDist $ reverseGraph graph   where
   maxEdgeDist edges = sum $ snd <$> take 1 (sortOn (negate.snd) edges)  

longestGraphPathBFS :: PathGraph -> Position -> Position -> Int -- ([Position], Int)
longestGraphPathBFS graph startPos endPos = go [Path {visited = [startPos], pDist = 0}] 0 where--paths startPos 0 where
  go :: [Path] -> Int -> Int
  go [] maxDist = maxDist
  go (activePath:remainingPaths) maxDist = let 
    nextPathsDist Path {visited = []} = ([], maxDist)
    nextPathsDist Path{visited = posHist@(cPos:vPos), pDist = cDist} 
      | cPos == endPos = ([], max maxDist cDist )
      | cDist + (maxGraphDist - distUpperBound maxGraph  posHist ) <= maxDist = ([], maxDist)  
      | otherwise = ([Path {visited =nPos:posHist, pDist = cDist + d} 
                      | (nPos, d) <- graph M.! cPos, nPos `notElem` vPos], maxDist)
    (newPaths, newMaxDist) = nextPathsDist activePath
     in go  (newPaths ++ remainingPaths) newMaxDist
  maxGraph = makeMaxGraph graph
  maxGraphDist = traceWInfo "maxGraphDist = " $ sum maxGraph


longestGraphPathDFS :: PathGraph -> Position -> Position -> Int -- ([Position], Int)
longestGraphPathDFS graph startPos endPos = St.execState (go (Path {visited = [startPos], pDist = 0})) 0 where--paths startPos 0 where
  go ::  Path  -> St.State Int ()
  go Path{visited = []} = return ()
  go Path{visited = posHist@(cPos:vPos), pDist = cDist}
    --nextPathsDist Path {visited = []} = ([], maxDist)
      | cPos == endPos = do 
          maxDist <- get
          put $ max cDist maxDist
          return  ()
      -- | cDist + (maxGraphDist - distUpperBound maxGraph  posHist ) <= maxDist = cDist
      | otherwise = do 
                     maxDist <- get 
                     if cDist + (maxGraphDist - distUpperBound maxGraph posHist ) <= maxDist then return () 
                     else let !nextPaths = [Path {visited = nPos:vPos, pDist = cDist +d} | (nPos, d) <- graph M.! cPos, nPos `notElem` vPos] 
                          in if null nextPaths then return () else mapM_ go nextPaths
     
  maxGraph = makeMaxGraph graph
  maxGraphDist = traceWInfo "maxGraphDist = " $ sum maxGraph

getSolutions23 :: String ->IO (Int, Int) -- PathGraph
getSolutions23 filename  = do 
  file <- readFile filename
  let grid = strToCharGrid file
  let pathGraph1 = makePathGraph  grid True
      pathGraph2 = makePathGraph  grid False 
      
      startPos = (1,2)
      endPos = let (_, (ymax, xmax)) = A.bounds grid in (ymax, xmax -1)
      noTrimPositions = [startPos, endPos]
      fullyTrim = reverseGraph.trimGraph noTrimPositions.reverseGraph
      sortGraph = M.map (sortOn (negate.snd)) -- could help a bit
  putStr $ unlines $ charGridToStrAug $ highlightJunctions grid
  return (longestGraphPathDFS (fullyTrim pathGraph1) startPos endPos, longestGraphPathBFS (sortGraph $ fullyTrim pathGraph2) startPos endPos)  

