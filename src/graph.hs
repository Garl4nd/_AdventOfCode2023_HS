{-# LANGUAGE OverloadedStrings #-}
import           Data.Graph.Inductive
import           Data.GraphViz
import Data.GraphViz.Printing ( renderDot )
import           Data.GraphViz.Attributes.Complete

import           Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                    as L
import         qualified  Data.Text.Lazy.IO                    as TIO
import           Data.Word
--import           WriteRunDot
import Data.Graph.Inductive
import Data.GraphViz
import Data.Foldable (traverse_)
import Control.Monad (zipWithM_)


ex1 :: Gr Text Text
ex1 = mkGraph [ (1,"one")
              , (3,"three")
              ]
              [ (1,3,"edge label") ]

ex1Params :: GraphvizParams n L.Text L.Text () L.Text
ex1Params = nonClusteredParams { globalAttributes = ga
                               , fmtNode          = fn
                               , fmtEdge          = fe
                               }
  where fn (_,l)   = [textLabel l]
        fe (_,_,l) = [textLabel l]

        ga = [ GraphAttrs [ RankDir   FromLeft
                          , BgColor   [toWColor White]
                          ]
             , NodeAttrs  [ shape     Circle
                          , FillColor (myColorCL 1)
                          , style     filled
                          ]
            ]   

ex3 :: G.DotGraph String
ex3 = digraph (Str "ex3") $ do

    graphAttrs [RankDir FromLeft]

    cluster (Num $ Int 0) $ do
        nodeAttrs               [shape DoubleCircle, FixedSize SetNodeSize, Width 1, style filled, myColor 1]
        node "Open"             [textLabel "open"]
        node "Closed"           [textLabel "closed"]

    cluster (Num $ Int 1) $ do
        nodeAttrs               [shape       Circle, FixedSize SetNodeSize, Width 1, style filled, myColor 1]
        node "ClosedWaitingAck" [textLabel "clsd waiting\nACK"]

    cluster (Num $ Int 2) $ do
        nodeAttrs               [shape     BoxShape,                 Width 1, style filled, myColor 3]
        node "cancel"           [textLabel "CANCEL"]
        node "cancelAck"        [textLabel "CANCEL_ACK"]

    "Open"             --> "cancel"
    "cancel"           --> "ClosedWaitingAck"
    "ClosedWaitingAck" --> "cancelAck"
    "cancelAck"        --> "Closed"


myColorCL :: Word8 -> ColorList
myColorCL n | n == 1 = c  (RGB 127 108 138)
            | n == 2 = c  (RGB 175 177 112)
            | n == 3 = c  (RGB 226 206 179)
            | n == 4 = c  (RGB 172 126 100)
            | otherwise = c  (RGB 255 255 255)
 where c rgb = toColorList [rgb]

x = node "mynode1" [style filled, color Blue,Shape Record, Label (StrLabel ( "{mynode1|<f0> one|<f1> two}") )]

--fNodes :: [(Integer, String)]
fNodes = [1..10]
sNodes = [11..20]
gr = digraph (Str "Test") $ do                     
                     graphAttrs [style filled, color LightGray, ordering InEdges]
                     Data.GraphViz.Types.Monadic.subgraph (Num $ Int 0) $ do
                           
                           sequence $ [node fNode [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack  $ show fNode ) ] | fNode <- fNodes]      
                     Data.GraphViz.Types.Monadic.subgraph (Num $ Int 1) $ do
                     --   graphAttrs [style filled, color LightGray, ordering OutEdges]
                        sequence $ [node sNode [style solid, color Red, Shape Triangle, Label (StrLabel $ L.pack $ show sNode ), ordering OutEdges] | sNode <- sNodes]
                     sequence $ Prelude.concat [[ fNode --> sNode | fNode <- fNodes] | sNode <- sNodes]
                        --node "mynode2" [style filled, color Blue,Shape Circle, Label (StrLabel ( "{mynode2|<f0> one|<f1> two}") )]]
                  --    node "mynode3" [style filled, color Blue,Shape Record, Label (StrLabel ( "{mynode3|<f0> one|<f1> two}") )]
                  --    edge "mynode1" "mynode2"
                  --       [ HeadPort (LabelledPort (PN ( "f0")) Nothing)
                  --             , TailPort (LabelledPort (PN ("f1")) Nothing)
                  --       ]
                  --    "mynode1:f0" --> "mynode2:f1"
                  --    "mynode1:f1" --> "mynode2:f1"

testFile1 = "broadcaster -> a, b, c\
\a -> b\
\b -> c\
\c -> inv\
\inv -> a"


gr2 = digraph (Str "AoC") $ do                     
                     graphAttrs [style filled, color LightGray, ordering InEdges]
                     node broadcaster [style dotted, color Black, Shape Square, Label (StrLabel $ L.pack broadcaster ) ] 
                     Data.GraphViz.Types.Monadic.subgraph (Num $ Int 0) $ do                           
                           sequence $ [node fNode [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack fNode ) ] | fNode <- flipFlops]      
                     Data.GraphViz.Types.Monadic.subgraph (Num $ Int 1) $ do
                     --   graphAttrs [style filled, color LightGray, ordering OutEdges]
                        sequence $ [node sNode [style solid, color Red, Shape Triangle, Label (StrLabel $ L.pack $ show sNode ), ordering OutEdges] | sNode <- conjunctions]
                     traverse_ ("broadcaster" -->) ["a", "b", "c"]
                     zipWithM_ (-->) ["a", "b", "c", "inv"] ["b","c","inv","a"]

gr3 :: G.DotGraph String
gr3 = digraph (Str "AoC2") $ do                     
                     graphAttrs [style filled, color LightGray, ordering InEdges]
                     node broadcaster [style dotted, color Black, Shape Square, Label (StrLabel $ L.pack broadcaster ) ] 
                     Data.GraphViz.Types.Monadic.subgraph (Num $ Int 0) $ do                           
                           sequence $ [node fNode [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack fNode ) ] | fNode <- ["a","b"]]      
                     Data.GraphViz.Types.Monadic.subgraph (Num $ Int 1) $ do
                     --   graphAttrs [style filled, color LightGray, ordering OutEdges]
                        sequence $ [node sNode [style solid, color Red, Shape Triangle, Label (StrLabel $ L.pack $ show sNode ), ordering OutEdges] | sNode <- ["inv", "con"]]
                     traverse_ ("broadcaster" -->) ["a"]
                     traverse_ ("a" -->) ["inv", "con"]
                     "inv" --> "b"
                     "b" --> "con"
                     "con" -->"output"

--                     zipWithM_ (-->) ["a", "b", "c", "inv"] ["b","c","inv","a"]



flipFlops = ["a", "b", "c"] 
conjunctions = ["inv"]
broadcaster = "broadcaster"


--graphToDotFileUnclustered :: gr l el -> p -> IO ()
graphToDotFileUnclustered :: Graph gr => gr l el -> FilePath -> IO ()
graphToDotFileUnclustered graph filename= TIO.writeFile filename $ (renderDot.toDot) $ graphToDot  nonClusteredParams graph

--graphToDotFileUnclustered :: Graph gr => gr l el -> FilePath -> IO ()
graphToDotFileUnclustered2 :: PrintDot p => p -> FilePath -> IO ()
graphToDotFileUnclustered2 graph filename= TIO.writeFile filename $ (renderDot.toDot)   graph
--graphToDotFile :: Graph gr => gr l el -> FilePath -> IO ()

graphToDotFile :: (Ord cl, Graph gr) => gr nl el -> GraphvizParams Node nl el cl l -> FilePath -> IO ()
graphToDotFile graph params filename= TIO.writeFile filename $ (renderDot.toDot) $ graphToDot  params graph

myColor :: Word8 -> Attribute
myColor n = Color $ myColorCL n

