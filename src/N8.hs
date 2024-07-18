module N8
  ( getSolutions8
  ) where

import Data.List (intersperse, isPrefixOf, isSuffixOf, transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import ParsingFuncs (splitOn, trimSpace)

data Node = Node
  { name :: String
  , left :: String
  , right :: String
  } deriving (Show)

data Direction
  = L
  | R
  deriving (Show, Read, Eq)

type NodeMap = M.Map String Node

parseFile :: String -> ([Direction], NodeMap)
parseFile file =
  ((read . return) <$> firstLine, M.fromList $ parseNodeLine <$> tail nodeLines)
  where
    firstLine:nodeLines = lines file
    parseNodeLine :: String -> (String, Node)
    parseNodeLine line = (nodeName, Node nodeName left right)
      where
        nodeName = trimSpace nodeNameStr
        [nodeNameStr, lrStr] = splitOn '=' line
        left = tail $ trimSpace leftPart
        right = trimSpace $ (init rightPart)
        [leftPart, rightPart] = splitOn ',' lrStr

path :: Node -> NodeMap -> [Direction] -> [Node]
path initNode nodeMap directions =
  scanl
    (goTo nodeMap)
    initNode
    $ cycle directions

goTo :: NodeMap ->  Node -> Direction -> Node
goTo nodemap (Node _ l r) direction = if direction == L then fromJust $ M.lookup l nodemap 
							else fromJust $ M.lookup r nodemap
walk :: NodeMap -> ([Node], [Direction]) -> ([Node], [Direction])
walk nodeMap (nodes, thisDirection:nextDirections) =
  (map (\node -> goTo nodeMap node thisDirection) nodes, nextDirections)

solution1 :: String -> Int
solution1 file = length $ aToZPath
  where
    (directions, nodeMap) = parseFile file
    initNode = fromJust $ M.lookup "AAA" nodeMap
    aToZPath =
      takeWhile (\node -> name node /= "ZZZ") $ drop 1$  path initNode nodeMap directions

solution2 :: String -> Int
solution2 file = length jointPathToZ
  where
    (directions, nodeMap) = parseFile file
    --aNodeKeys = filter  ("A" `isSuffixOf`) (M.keys nodeMap)
    --zNodeKeys = filter  ("Z" `isSuffixOf` ) (M.keys nodeMap)
    aNodes = M.toList $ M.filter (\node -> "A" `isSuffixOf` name node) nodeMap
    paths =
      map (\(_, initNode) -> path initNode nodeMap ( directions)) aNodes
    jointPath = transpose paths
    jointPathToZ =
      takeWhile (not . all (\node -> "Z" `isSuffixOf` name node)) jointPath
    --aToZPath = takeWhile (\node -> name node /= "ZZZ")  $ path initNode nodeMap directions

solution2' :: String -> Int
solution2' file = length jointPathToZ
  where
    (directions, nodeMap) = parseFile file
    --aNodeKeys = filter  ("A" `isSuffixOf`) (M.keys nodeMap)
    --zNodeKeys = filter  ("Z" `isSuffixOf` ) (M.keys nodeMap)
    aNodes = drop 1 $ take 3 $ snd <$> (M.toList $ M.filter (\node -> "A" `isSuffixOf` name node) nodeMap)
    paths =  fst <$> iterate (walk nodeMap) (aNodes, cycle directions)
    jointPathToZ =
      takeWhile (not . all (\node -> "Z" `isSuffixOf` name node)) paths

solution2'' :: String -> Int
solution2'' file = foldl lcm 1 iters
  where
    (directions, nodeMap) = parseFile file
    --aNodeKeys = filter  ("A" `isSuffixOf`) (M.keys nodeMap)
    --zNodeKeys = filter  ("Z" `isSuffixOf` ) (M.keys nodeMap)
    aNodes = M.toList $ M.filter (\node -> "A" `isSuffixOf` name node) nodeMap
    paths = map (\(_, initNode) -> path initNode nodeMap ( directions)) aNodes
    iters = length <$> map (takeWhile (\node ->not $ "Z" `isSuffixOf` name node)) paths
    jointPath = transpose paths
    jointPathToZ =
      takeWhile (not . all (\node -> "Z" `isSuffixOf` name node)) jointPath
 
solution2'debug :: String -> [[(Node, Int)]]
solution2'debug file = targetNodes
  where
    (directions, nodeMap) = parseFile file
    --aNodeKeys = filter  ("A" `isSuffixOf`) (M.keys nodeMap)
    --zNodeKeys = filter  ("Z" `isSuffixOf` ) (M.keys nodeMap)
    aNodes = snd <$> (M.toList $ M.filter (\node -> "A" `isSuffixOf` name node) nodeMap)
    paths =  map (\initNode -> path initNode nodeMap ( directions)) aNodes
    targetNodes =  [take 5 $ filter (isTarget.fst) $ zip path [0..] | path <- paths] 
    isTarget node=  "Z" `isSuffixOf` name node


solution2dbg :: String -> [[Node]] --([Node], Int) --[[Node]]
solution2dbg file = jointPathToZ
  where
    (directions, nodeMap) = parseFile file
    --aNodeKeys = filter  ("A" `isSuffixOf`) (M.keys nodeMap)
    --zNodeKeys = filter  ("Z" `isSuffixOf` ) (M.keys nodeMap)
    aNodes =
      take 2 $ M.toList $ M.filter (\node -> "A" `isSuffixOf` name node) nodeMap
    paths = map (\(_, initNode) -> path initNode nodeMap directions) aNodes
    jointPath = transpose paths
    jointPathToZ =
      takeWhile (not . all (\node -> "Z" `isSuffixOf` name node)) jointPath
    --aToZPath = takeWhile (\node -> name node /= "ZZZ")  $ path initNode nodeMap directions

getSolutions8 :: String -> IO (Int, Int)
getSolutions8 filename = do
  file <- readFile filename
  let res = parseFile file
    --print $ res
    --let (directions, nodeMap) = res
    --let initNode = fromJust $ M.lookup "AAA" nodeMap
    --let myPath = takeWhile (\node -> name node /= "ZZZ")  $ path initNode nodeMap directions
    --print myPath
  --print $ solution2'debug file
    --print $ last $  intersperse  [] $  solution2dbg file
  return (solution1 file, solution2'' file)
