module N25 
 (getSolutions25) where 

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import Data.List (unfoldr, find, nub, sortOn)
import Data.Array ((!))
import ParsingFuncs (splitOn, groupByUnique)
import GraphMaker (writeUndirectedGraph)
import Data.Maybe (isJust, fromJust)
import System.Random
import Control.Monad (forM)
type SetGraph k = M.Map k (S.Set k)
type StringGraph = M.Map String (S.Set String)

makeWireGraph :: String -> StringGraph
makeWireGraph file = foldl (M.unionWith (S.union)) M.empty $ graphForLine <$> lines file  where
    graphForLine :: String -> StringGraph
    graphForLine line =  let 
                            [source, allTgs] = splitOn ':' line                            
                            tgs = words allTgs                        
                         in M.fromList $ (source, S.fromList tgs):[(tg, S.singleton source) | tg <- tgs]

undirectedEdges :: Ord k => M.Map k (S.Set k) -> S.Set (S.Set k)
undirectedEdges  = M.foldlWithKey (\acc source tgSet -> S.union acc $
  S.map (\tg -> S.fromList [source, tg] )  tgSet) S.empty 

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

chooseEdges :: S.Set (S.Set k) -> Int -> [[S.Set k]]
chooseEdges edges n = subsequencesOfSize n $ S.toList edges 

rem3Graphs :: Ord k => SetGraph k -> [SetGraph k]
rem3Graphs graph = let 
   edgeL = S.toList (undirectedEdges graph)
   edgeA = A.listArray (0, e) edgeL
   e = length edgeL - 1
   in [ removeEdge (edgeA ! m) $ removeEdge (edgeA ! l) $ removeEdge (edgeA ! k) graph 
    | k<- [0..e], l<- [(k+1)..e], m <- [(l+1)..e] ]
  

   --rem1Graphs = removeEdge graph <$> edgeList
   --rem2Graphs = concat [[removeEdge (rem1Graphs !! k) (edgeList !! l) | l<- [k..length edgeList] ]| k <- [0..length edgeList] ]
  --in graph

removeEdge :: Ord k => S.Set k -> SetGraph k -> SetGraph k
removeEdge edgeForRemoval graph = let [s, e] = S.toList edgeForRemoval 
                                      in M.adjust (S.delete e) s $ M.adjust (S.delete s) e graph

connectedComponents :: Ord k => SetGraph k -> [S.Set k] 
connectedComponents graph = unfoldr findConnected S.empty where
  findConnected allVisitedNodes = case find (`notElem` allVisitedNodes) $ M.keys graph of 
   Nothing -> Nothing
   Just seedNode -> go (S.singleton seedNode) S.empty where
    go boundary currentSet = if 
       | S.null boundary -> Just (currentSet, S.union allVisitedNodes currentSet)  
       | otherwise -> go newBoundary newSet where
     newBoundary = S.filter (`S.notMember` newSet) $ S.unions (S.map (graph M.!) boundary)
     newSet = S.union currentSet boundary

findPath :: Ord k => SetGraph k -> k -> k -> Maybe [(k,k)]
findPath graph startNode endNode = reverse <$> go [startNode] emptyVisitMap where
    emptyVisitMap = (const Nothing) <$> graph
    go boundary currentVisitMap = if 
       | null boundary -> Nothing
       | isJust $ currentVisitMap M.! endNode -> Just  $ recreatePath startNode endNode currentVisitMap   
       | otherwise -> go newBoundary newVisitMap where 
     newBoundary = nub $ snd <$> newCrosses
     newCrosses = concatMap 
      (\ source -> [(source, tg) | tg <- S.toList  (graph M.! source), currentVisitMap M.! tg == Nothing]  ) 
                           boundary
     newVisitMap = foldl (\acc (source, tg) -> M.insert tg (Just source) acc ) 
      currentVisitMap newCrosses
    recreatePath startNode endNode visitMap = unfoldr (\k -> case M.lookup k visitMap of 
       Nothing -> Nothing
       Just Nothing -> Nothing
       Just (Just t) -> if k == startNode then Nothing else Just ((t, k), t)) endNode  


 


          

writeGraph :: String -> String -> IO ()
writeGraph filename graphName = do 
    file <- readFile filename
    let wireGraph = makeWireGraph file
    print $ show $ undirectedEdges wireGraph
    print $ length $ undirectedEdges wireGraph
    writeUndirectedGraph graphName wireGraph
    print $ connectedComponents wireGraph
   -- writeUndirectedGraph graphName $ last (rem3Graphs wireGraph ) 


writeCutGraph :: String -> String -> IO ()
writeCutGraph filename graphName = do 
    file <- readFile filename
    let wireGraph = makeWireGraph file
    print $ show $ undirectedEdges wireGraph
    print $ length $ undirectedEdges wireGraph
    --let cutGraph = foldl (flip removeEdge) wireGraph $ S.fromList <$>  [["nvd", "jqt"], ["bvb", "cmg"], ["hfx", "pzl"]] 
    let cutGraph = foldl (flip removeEdge) wireGraph $ S.fromList <$>  [["jmn", "zfk"],  ["hvm", "grd"], ["pmn", "kdc"]]
    writeUndirectedGraph graphName cutGraph
    let connectedComps = connectedComponents cutGraph
    print $ length <$> connectedComps
    print $ findPath wireGraph "ntq" "rsh" 
    print $ product $ length <$> connectedComps

getSolutions25 :: String -> IO (Int, Int)
getSolutions25 filename = do 
   file <- readFile filename
   let wireGraph = makeWireGraph file
       keys = M.keys wireGraph
   randomNumbers::[(Int,Int)] <- forM [1..1000] $ const $ randomRIO ((0,0), ( (length keys) -1, (length keys) -1 ))
--   randomNumbers2 <- forM [1..200] $ randomRIO (0, length keys)
   let paths =   fromJust <$> map (\(i,j) -> findPath wireGraph (keys !! i ) (keys !! j))  randomNumbers
       pathSets = map (map (\(s, e) -> S.fromList [s,e])) paths
       edgesHistogram = sortOn (negate.snd) .  groupByUnique id $ concat pathSets  
       mostCommon3Edges = fst <$> (take 3  edgesHistogram)
       cutGraph = foldl (flip removeEdge) wireGraph mostCommon3Edges 
       connectedComps = connectedComponents cutGraph 
   return (product $ length <$> connectedComps, 2023)
