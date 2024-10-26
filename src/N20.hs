{-# Language NamedFieldPuns #-}



module N20 (getSolutions20) where 

import ParsingFuncs (splitOn, trimSpace, splitBySubstr, countIf)

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
import Data.Foldable (traverse_, Foldable (toList))
import Data.List (unfoldr, intercalate)
import qualified Data.Sequence as S
import System.TimeIt
import Data.Sequence (ViewL ((:<)), ViewR ((:>)), (|>), (><))

import qualified Data.Foldable

class ModuleClass a where 
    respondToSignal ::  a -> Signal ->  (a, SignalSeq)
    targetModules :: a -> [String]
    name :: a -> String

sendSigToTargets :: ModuleClass a => a-> SignalType -> SignalSeq
sendSigToTargets mod sigType= S.fromList [Signal {from = name mod, to = target, sigType = sigType } | target <- targetModules mod]

type ModMap = M.Map String Module
data SignalType = High | Low deriving (Eq, Show)
data Signal = Signal {from:: String, to :: String, sigType :: SignalType} deriving (Eq, Show)

data FFState = On | Off deriving (Show, Eq)
data Broadcaster = Broadcaster {bTargets :: [String], bName :: String} deriving Show
data FlipFlop = FlipFlop {ffTargets :: [String], ffName :: String, state :: FFState} deriving Show
data Conjunction = Conjuction {cTargets :: [String], cName :: String, pulseMemory ::  M.Map String SignalType} deriving Show
data Module = BR Broadcaster | FL FlipFlop | CJ Conjunction  deriving Show

instance ModuleClass Broadcaster where    
    respondToSignal  bs (Signal {sigType}) = (bs, sendSigToTargets bs sigType  )            
    targetModules Broadcaster {bTargets} = bTargets
    name  = bName


instance ModuleClass FlipFlop where        
    respondToSignal  ff (Signal {sigType = High}) = (ff, S.empty)
    respondToSignal  ff@FlipFlop {state} (Signal {sigType = Low}) = case state of
        On -> (ff{state = Off}, sendSigToTargets ff Low)
        Off -> (ff{state = On}, sendSigToTargets ff High)
    targetModules = ffTargets    
    name = ffName

instance ModuleClass Conjunction where
    respondToSignal  cj@Conjuction{pulseMemory}  (Signal {sigType, from} ) = let newPulseMemory = M.insert from sigType pulseMemory 
                                                                                 outSigType = if all (== High) newPulseMemory then Low else High in 
                                                                                    (cj{pulseMemory = newPulseMemory}, sendSigToTargets cj outSigType)
    targetModules = cTargets
    name = cName

instance ModuleClass Module where
    respondToSignal = \case  
                (BR x) -> go x BR  
                (CJ x) -> go x CJ  
                (FL x) -> go  x FL 
            where
        go:: ModuleClass a => a ->  (a->Module ) ->  Signal -> (Module, SignalSeq)
        go x constr sig   = let (x', sigs) = respondToSignal x sig in (constr x', sigs)  

    name (BR x) = name x
    name (CJ x) = name x
    name (FL x) = name x

    targetModules (BR x) = targetModules x
    targetModules (CJ x) = targetModules x
    targetModules (FL x) = targetModules x

buttonPushLs :: ModMap -> (ModMap, [Signal])
buttonPushLs modMap = propagateSignalsLs modMap $ [Signal {from = "Button", to = "broadcaster", sigType = Low}]

buttonPushNTimesLs :: ModMap -> Int -> (ModMap, [Signal])
buttonPushNTimesLs modMap n = let  propHist = unfoldr  (\ (i, cMap)  -> if i > n then Nothing else  let (updatedMap, sigHist) = buttonPushLs cMap in Just ((updatedMap, sigHist), (i+1, updatedMap))) (1, modMap)  in
    (fst $ last propHist, concat $ snd <$> propHist)


buttonPush :: ModMap -> (ModMap, SignalSeq)
buttonPush modMap = propagateSignalsSeq modMap $ S.singleton  Signal {from = "Button", to = "broadcaster", sigType = Low}

buttonPushNTimes :: ModMap -> Int -> (ModMap, SignalSeq)
buttonPushNTimes modMap n = let  propHist = S.unfoldr  (\ (i, cMap)  -> if i > n then Nothing else  let (updatedMap, sigHist) = buttonPush cMap in Just ((updatedMap, sigHist), (i+1, updatedMap))) (1, modMap)  in
    (fst $ let (S.viewr -> _ :> r) = propHist in r , foldl (<>) S.empty $ snd <$> propHist)

buttonPushInfinite :: ModMap ->  [(Int, ModMap, SignalSeq)]
buttonPushInfinite modMap = iterate  (\ (i, cMap, _)  ->  let (updatedMap, sigHist) = buttonPush cMap in (i+1, updatedMap,  sigHist )) (0, modMap, S.empty)  

propagateSignalsLs :: ModMap -> [Signal] -> (ModMap, [Signal]) 
propagateSignalsLs modMap signals =   propagate modMap [] signals  where --foldl propagateSingle modMap signals where 
    propagate :: ModMap -> [Signal] -> [Signal] -> (ModMap, [Signal])
    propagate cMap sigHistory [] = (cMap, sigHistory)   --sig@Signal {to} = let mod = cMap ! to
    propagate cMap sigHistory (cSig@Signal {to}:signalQueue) = case M.lookup to cMap  of
            Nothing -> propagate cMap (sigHistory++[cSig]) (signalQueue) 
            Just mod -> let  (updatedModule, newSignals) = respondToSignal mod cSig
                             updatedMap = M.insert to updatedModule cMap 
                        in propagate updatedMap (sigHistory++[cSig]) (signalQueue ++ toList newSignals) 

type SignalSeq = S.Seq Signal

propagateSignalsSeq :: ModMap -> SignalSeq-> (ModMap, SignalSeq) 
propagateSignalsSeq modMap signals =   propagate modMap S.empty signals  where --foldl propagateSingle modMap signals where 
    propagate :: ModMap -> SignalSeq  -> SignalSeq-> (ModMap, SignalSeq)
    propagate cMap sigHistory (S.viewl -> S.EmptyL)  = (cMap, sigHistory)   --sig@Signal {to} = let mod = cMap ! to    
    propagate cMap sigHistory (S.viewl -> cSig@Signal {to} :< signalQueue) = case M.lookup to cMap  of
            Nothing -> propagate cMap (sigHistory |> cSig) signalQueue            
            Just mod -> let  (updatedModule, newSignals) = respondToSignal mod cSig
                             updatedMap = M.insert to updatedModule cMap 
                        in propagate updatedMap (sigHistory |> cSig) (signalQueue ><  newSignals) 

parseFile :: String -> ModMap
parseFile file = foldl (\cMap (modName, mod) -> case mod of 
                    CJ conjMod -> M.insert modName (initMemory conjMod) cMap
                    _ -> cMap)  initMap modList 
                    where 
    initMemory conjMod  = let sourceMods = filter (\mod -> name conjMod `elem` targetModules mod) $ snd <$> modList
                              in CJ conjMod {pulseMemory = M.fromList [(name sourceMod, Low) | sourceMod <- sourceMods]}
    initMap = M.fromList modList
    modList = map (\mod -> (name mod, mod)) $  processLine <$>  lines file
    processLine :: String -> Module 
    processLine line = let 
        [typeName, targetList]  = splitBySubstr "->" line
        (modType:modName) = typeName
        targets = splitOn ',' targetList in 
            case modType of 
                '%' -> FL $ FlipFlop {ffTargets = trimSpace<$> targets, ffName = trimSpace modName, state = Off}
                '&' -> CJ $ Conjuction {cTargets = trimSpace <$> targets, cName = trimSpace modName, pulseMemory = M.empty}
                _   -> BR $ Broadcaster {bTargets = trimSpace <$> targets, bName = trimSpace typeName}


buttonPushFromFile :: String -> IO () -- (ModMap, [Signal])
buttonPushFromFile filename =  do
    file <- readFile filename
    let res =  buttonPush $ parseFile file        
    putStr $ intercalate "\n" $ toList $   show <$> snd  res

buttonPushFromFileN :: String -> Int -> IO () -- (ModMap, [Signal])
buttonPushFromFileN filename n =  do
    file <- readFile filename
    let res =  buttonPushNTimes  (parseFile file ) n
    putStr $ intercalate "\n" $  toList $  show <$> snd res


solution1 :: String -> Int
solution1 file = let 
                    mMap = parseFile file
                    (_, sigHist) =  buttonPushNTimes mMap 1000 
                    lowSignals =  length $ S.filter ((Low==).sigType)  sigHist
                    highSignals =   length $ S.filter  ((High==).sigType)  sigHist
                 in lowSignals *highSignals

solution1Ls :: String -> Int
solution1Ls file = let 
                    mMap = parseFile file
                    (_, sigHist) =  buttonPushNTimesLs mMap 1000
                    lowSignals =  countIf ((Low==).sigType)  sigHist
                    highSignals =   countIf ((High==).sigType)  sigHist
                 in lowSignals *highSignals

solution2 :: String -> Int
solution2 file = let 
                    mMap = parseFile file                    
                    tjPrevNodes = M.keys $ pulseMemory $ let CJ cMod =  mMap ! "tj" in cMod
                    highPulseArrival modName = case dropWhile (\(_, _, sigHist ) -> Signal {from = modName, to = "tj", sigType = High} `notElem` sigHist) $ buttonPushInfinite mMap  of
                        [] -> 0
                        lst ->  (\(it, _, _) -> it).head $ lst                    
                 in foldl lcm 1 $ highPulseArrival <$>  tjPrevNodes  

getSolutions20 :: String -> IO (Int, Int) 
getSolutions20 filename =  do
    file <- readFile filename
    timeItNamed "SeqVersion" $ print $ solution1 file
    timeItNamed "Ls version" $ print $ solution1Ls file
    --return (0,0)
    return (solution1 file,  solution2 file)


-- graph visualisation functions:

graphToDotFileUnclustered :: PrintDot p => p -> FilePath -> IO ()
graphToDotFileUnclustered graph filename= LIO.writeFile filename $ (renderDot.toDot)   graph

writeGraph :: String -> String -> IO ()
writeGraph file filename = graphToDotFileUnclustered graph filename  where
    graph = makeGraph modMap 
    modMap = parseFile file

genGraph :: String -> IO ()
genGraph filename = do 
    file <- readFile filename
    let [_, fname] = splitOn '\\' filename
        [sanitizedName , _] = splitOn '.' fname 
    writeGraph file $ "graphviz\\"<>sanitizedName<>".dot"



makeGraph :: ModMap -> G.DotGraph String
makeGraph modMap  =     
    digraph (Str "AoC 20") $ do                     
                    graphAttrs [style filled, color LightGray, ordering InEdges]                    
                    sequence_ [node flipFlop [style solid, color Blue, Shape Circle, Label (StrLabel $ L.pack  flipFlop )] | flipFlop <- fst <$> flipFlops ] 
                    sequence_ [node cjs [style solid, color Red, Shape Square, Label (StrLabel $ L.pack  cjs )] | cjs <- fst <$> conjunctions ] 
                    sequence_ [node broadcaster [style solid, color Black, Shape Triangle, Label (StrLabel $ L.pack  broadcaster )] | broadcaster <- fst <$> broadcasters ] 
                    forM_ flipFlops $ \(name, mod )  -> traverse_ (\tg -> edge name tg [color Blue]  ) $  targetModules mod
                    forM_ conjunctions $ \(name, mod )  -> traverse_ (\tg -> edge name tg [color Red]  ) $  targetModules mod
                    forM_ broadcasters $ \(name, mod )  -> traverse_ (\tg -> edge name tg [color Black]  ) $  targetModules mod

    where
        flipFlops = M.toList $ M.filter (\case {(FL _) -> True; _ -> False} ) modMap
        conjunctions = M.toList $ M.filter (\case {(CJ _) -> True; _ -> False} ) modMap
        broadcasters = M.toList $ M.filter (\case {(BR _) -> True; _ -> False} ) modMap
                     