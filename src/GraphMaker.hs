{-# Language TupleSections #-}
module GraphMaker 
  (writeLabeledGraph, writeLabeledUndirectedGraph, writeUndirectedGraph) where

import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised   as G
import qualified  Data.Text.Lazy  as L    
import qualified  Data.Text.Lazy.IO                    as LIO
import Data.GraphViz.Attributes.Complete
import Data.GraphViz
import Data.GraphViz.Printing ( renderDot )
import qualified Data.Map as M
import Data.Map ((!))
import Control.Monad (forM_)

import Data.Foldable (traverse_)
import ParsingFuncs (splitOn, trimSpace, splitBySubstr, countIf)


graphToDotFileUnclustered :: PrintDot p => p -> FilePath -> IO ()
graphToDotFileUnclustered graph filename= LIO.writeFile filename $ (renderDot.toDot)   graph

writeLabeledGraph :: (Show a, Ord a, Show l) => String -> M.Map a [(a,l)] -> IO ()
writeLabeledGraph filename mapGraph = graphToDotFileUnclustered graph filename  where
    graph = makeLabeledGraph mapGraph    

writeLabeledUndirectedGraph :: (Show a, Ord a, Eq l, Show l) => String -> M.Map a [(a,l)] -> IO ()
writeLabeledUndirectedGraph filename mapGraph = graphToDotFileUnclustered graph filename  where
    graph = makeLabeledUndirectedGraph mapGraph    

writeUndirectedGraph :: (Show a, Ord a, Foldable m) => String -> M.Map a (m a) -> IO ()
writeUndirectedGraph filename mapGraph = graphToDotFileUnclustered graph filename  where
    graph = makeUndirectedGraph mapGraph    

-- genGraph :: String -> IO ()
-- genGraph filename = do 
--     file <- readFile filename
--     let [_, fname] = splitOn '\\' filename
--         [sanitizedName , _] = splitOn '.' fname 
--     writeGraph file $ "graphviz\\"<>sanitizedName<>".dot"

--reverseGraph :: PathGraph -> PathGraph
reverseLabeledGraph graph = M.foldrWithKey (\pos edges accMap ->  foldl (\accMap' (tg, dist) -> M.insertWith (++) tg [(pos, dist)] accMap'  ) accMap edges) (M.fromList $ (, []) <$> M.keys graph) graph



makeLabeledGraph :: (Show a, Ord a, Show l, Foldable m) => M.Map a (m (a,l))  -> G.DotGraph String
makeLabeledGraph mapGraph  =     
    digraph (Str "AoC 20") $ do                     
                    graphAttrs [style filled, color LightGray, ordering InEdges]                    
                    sequence_ [node nodeLabel [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack  nodeLabel )] | nodeLabel <- show <$> M.keys mapGraph ] 
                    forM_ (M.keys mapGraph) $ \name  -> traverse_ (\(tg, val) -> edge (show name) (show tg) [color Black, toLabel $ show val]  ) $   mapGraph ! name 

makeLabeledUndirectedGraph :: (Show a, Ord a, Eq l, Show l, Foldable m) => M.Map a (m (a,l))  -> G.DotGraph String
makeLabeledUndirectedGraph mapGraph  =     
    graph (Str "AoC 20") $ do                     
                    graphAttrs [style filled, color LightGray, ordering InEdges]                    
                    sequence_ [node nodeLabel [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack  nodeLabel )] | nodeLabel <- show <$> M.keys mapGraph ] 
                    forM_ (M.keys mapGraph) $ \name  -> traverse_ (\(tg, val) -> 
                      if tg `M.member` reversedGraph && (name, val) `elem` reversedGraph ! tg && tg<= name then return ()  
                      else edge (show name) (show tg) [color Black, toLabel $ show val]  ) $   mapGraph ! name where

    reversedGraph = reverseLabeledGraph mapGraph

makeUndirectedGraph :: (Show a, Ord a, Foldable m) => M.Map a (m a)  -> G.DotGraph String
makeUndirectedGraph mapGraph  =     
    graph (Str "AoC 20") $ do                     
                    graphAttrs [style filled, color LightGray, ordering InEdges]                    
                    sequence_ [node nodeLabel [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack  nodeLabel )] | nodeLabel <- show <$> M.keys mapGraph ] 
                    forM_ (M.keys mapGraph) $ \name  -> traverse_ (\tg -> 
                      if tg `M.member` mapGraph && name `elem` mapGraph M.! tg  &&  tg <= name  then return () --tg `M.member` reversedGraph && name `elem` reversedGraph ! tg && tg<= name then return ()  
                      else edge (show name) (show tg) [color Black]  ) $   mapGraph ! name 

    --reversedGraph = reverseLabeledGraph mapGraph
                    

                   


                     
