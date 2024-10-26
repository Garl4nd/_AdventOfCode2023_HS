{-# Language NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}


module N20_bck () where 

import ParsingFuncs (splitOn, trimSpace, splitBySubstr)
import           Data.GraphViz.Types.Monadic
import qualified           Data.Text.Lazy                    as L    
import qualified  Data.Text.Lazy.IO                    as LIO
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz
import Data.GraphViz.Printing ( renderDot )
import qualified Data.Map as M
type ModMap = M.Map String Module

class ModuleClass a where 
    respondToSignal ::  a -> Signal ->  (a, [Signal])
    targetModules :: a -> [String]
    name :: a -> String

sendSigToTargets :: ModuleClass a => a-> SignalType -> [Signal]
sendSigToTargets mod sigType= [Signal {from = name mod, to = target, sigType = sigType } | target <- targetModules mod]


instance ModuleClass Broadcaster where    
    respondToSignal  bs (Signal {sigType}) = (bs, sendSigToTargets bs sigType  )            
    targetModules Broadcaster {bTargets} = bTargets
    name  = bName


instance ModuleClass FlipFlop where        
    respondToSignal  ff (Signal {sigType = High}) = (ff, [])
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
    respondToSignal mod = case mod of 
                (BR x) -> \sig -> (CJ {}, [])
                (CJ x) -> go x CJ  
                (FL x) -> go  x FL 
            where
        go:: ModuleClass a => a ->  (a->Module ) ->  Signal -> (Module, [Signal])
        go x constr sig   = let (x', sigs) = respondToSignal x sig in (constr x', sigs)  
    name (BR x) = name x
    name (CJ x) = name x

    --respondToSignal (CJ x)  =  f CJ
  --  respondToSignal (FL x) sig = f x sig FL 
    
    --respondToSignal sig (BR b) = BR $ respondToSignal sig b  
    --respondToSignal (BR b) = BR $ respondToSignal b  

data SignalType = High | Low deriving (Eq, Show)
data Signal = Signal {from:: String, to :: String, sigType :: SignalType} deriving Show

data FFState = On | Off deriving (Show, Eq)
data Broadcaster = Broadcaster {bTargets :: [String], bName :: String} deriving Show
data FlipFlop = FlipFlop {ffTargets :: [String], ffName :: String, state :: FFState} deriving Show
data Conjunction = Conjuction {cTargets :: [String], cName :: String, pulseMemory ::  M.Map String SignalType} deriving Show
data Module = BR Broadcaster | FL FlipFlop | CJ Conjunction  deriving Show

parseFile :: String -> ModMap
parseFile file = M.fromList $ map (\mod -> (name mod, mod)) $  processLine <$>  lines file where 
    processLine :: String -> Module
    processLine line = let 
        [typeName, targetList]  = splitBySubstr "->" line
        (modType:modName) = typeName
        targets = splitOn ',' targetList in 
            case modType of 
                '%' -> FL $ FlipFlop {ffTargets = targets, ffName = trimSpace modName, state = Off}
                '&' -> CJ $ Conjuction {cTargets = targets, cName = trimSpace modName, pulseMemory = M.empty}
                _   -> BR $ Broadcaster {bTargets = targets, bName = typeName}


--gr :: ModMap -> DotGraph String
gr modMap  = digraph (Str "AoC 20") $ do                     
                     graphAttrs [style filled, color LightGray, ordering InEdges]
                     node "a" [style dotted, color Black, Shape Square, Label (StrLabel $ L.pack "a" ) ] 
                     



testFile1 = "\
\broadcaster -> a, b, c\
\%a -> b\
\%b -> c\
\%c -> inv\
\&inv -> a"
graphToDotFileUnclustered :: PrintDot p => p -> FilePath -> IO ()
graphToDotFileUnclustered graph filename= LIO.writeFile filename $ (renderDot.toDot)   graph
