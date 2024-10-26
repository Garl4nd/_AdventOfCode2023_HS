--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module N19
 (getSolutions19) where

import Debug.Trace
import ParsingFuncs (splitOn, splitBySubstr)
import Data.List (uncons, sortOn, foldl', sortBy)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.Map ((!))

unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

data Property = X | M | A | S deriving (Show, Eq, Ord)
type Interval = (Int, Int)
data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving Show
data PartInt = PartInt {xInt :: Interval, mInt :: Interval, aInt :: Interval, sInt :: Interval} deriving Show

data CompType = L | G deriving (Show, Eq, Ord)
data FullRule = FullRule {prop :: Property,  compType :: CompType, value :: Int, dest :: String} deriving Show
data Workflow = Workflow {name:: String, fullRules :: [FullRule], fallback :: String } deriving Show
type WfMap = M.Map String Workflow


getPartProp :: Property -> (Part -> Int)
getPartProp X = x
getPartProp M = m
getPartProp A = a
getPartProp S = s

getPartPropInt :: Property -> (PartInt -> Interval)
getPartPropInt X = xInt
getPartPropInt M = mInt
getPartPropInt A = aInt
getPartPropInt S = sInt

readProperty :: Char -> Property
readProperty 'x' = X
readProperty 'm' = M
readProperty 'a' = A
readProperty 's' = S

parseRule :: String -> FullRule
parseRule str =  let propChar = head str 
                     compType = if str !! 1 == '<' then L else G
                     [val, dest] = splitOn ':' $ drop 2 str                         
                 in  FullRule (readProperty propChar) compType (read val) dest

parseWorkflow :: String -> Workflow
parseWorkflow line  = let  [name, allRules] = splitOn '{' line 
                           rules = splitOn ',' $ init allRules
                           (fullRuleStrs, fallback) = fromJust $ unsnoc rules
                           fullRules = parseRule <$> fullRuleStrs
                      in  Workflow name fullRules fallback

parsePart :: String -> Part
parsePart str = let propertyStrs = splitOn ',' $ init $ tail str
                    parseProperty propertyStr = (readProperty $ head propertyStr, read $ drop 2 propertyStr)  
                    properties = parseProperty <$> propertyStrs
                    updatePart part (property, val) = case property of
                                                X -> part {x = val}
                                                M -> part {m = val}
                                                A -> part {a = val}
                                                S -> part {s = val}
                    in foldl updatePart Part {x=0, m =0, a=0, s=0} properties

parseFile :: String -> (WfMap, [Part])                           
parseFile file = let [workflowStrs, partStrs] = splitBySubstr "\n\n" file
                     workflows = parseWorkflow <$> lines workflowStrs
                     parts = parsePart <$> lines partStrs 
                 in (M.fromList $ (\workflow -> (name workflow, workflow)) <$> workflows, parts)

satisfies :: Part -> FullRule -> Bool
satisfies part (FullRule {prop = prop, compType = compType, value = value} ) = let compFunc L = (<) 
                                                                                   compFunc G = (>)
                                                                                   partVal = getPartProp prop part
                                                                               in compFunc compType  partVal value
isAccepted' ::  WfMap -> String -> Part -> Bool
isAccepted' wfMap initWF part = go initWF where
  go wfName = case nextWorkFlow   (wfMap ! wfName) part of
                    "R" -> False
                    "A" -> True
                    nextName -> go nextName
              
nextWorkFlow :: Workflow -> Part -> String
nextWorkFlow wf part  = go (fullRules wf) where
  go [] = fallback wf
  go (currentRule:rest) = if part `satisfies` currentRule then dest currentRule else go rest 

score :: Part -> Int
score (Part x m a s)  =  x + m + a + s

intervalMsize :: PartInt -> Int
intervalMsize (PartInt (x1, x2) (m1,m2) (a1,a2) (s1,s2)) = (x2-x1+1)*(m2-m1+1)*(a2-a1+1)*(s2-s1+1)

solutionsForWf :: WfMap -> String -> PartInt -> Int
solutionsForWf wfMap wfName partInts = sum solutionsPerBranch  where
 wf@Workflow{fullRules = ruleList, fallback = fallback} = wfMap ! wfName 
 solutionsPerBranch = map (\(acceptInt, branchWfName) -> case branchWfName of 
   "A" -> intervalMsize acceptInt
   "R" -> 0
   _ -> solutionsForWf wfMap branchWfName acceptInt) $ acceptBranches partInts ruleList
 acceptBranches :: PartInt -> [FullRule] -> [(PartInt, String)]
 acceptBranches curInts [] = [(curInts, fallback)]
 acceptBranches curInts (curRule:rest) = let (acceptInt, rejectInt) = splitInts curRule curInts in
                                              (acceptInt, dest curRule):(acceptBranches rejectInt rest)
                                             
splitInts :: FullRule -> PartInt -> (PartInt, PartInt)
splitInts FullRule{prop = prop, value = val, compType = ct} partInts = case prop of 

  X -> (partInts {xInt=acceptInt}, partInts {xInt= rejectInt})
  M -> (partInts {mInt=acceptInt}, partInts {mInt= rejectInt})
  A -> (partInts {aInt=acceptInt}, partInts {aInt= rejectInt})
  S -> (partInts {sInt=acceptInt}, partInts {sInt= rejectInt})
  where

  (l, r) = getPartPropInt prop  $ partInts
  (acceptInt, rejectInt) = if ct == L then ((l,val-1), (val, r)) else ((val+1, r), (l, val))

solution1 :: String -> Int
solution1 file = sum $ score <$> filter (isAccepted' wfMap "in") parts where
  (wfMap, parts) = parseFile file

solution2 :: String -> Int
solution2 file = solutionsForWf wfMap "in" $ PartInt (1,4000) (1, 4000) (1,4000) (1,4000) where 
 (wfMap, _) = parseFile file

getSolutions19 :: String -> IO (Int, Int)
getSolutions19  filename = do 
  file <- readFile filename  
  return (solution1 file, solution2 file) -- solution1 file, solution2 file) 

--genIntervals :: [(Int, CompType)] -> [(Int, Int)]
--genIntervals vals = filter (\(a,b) -> b>=a ) $ zipWith (\(a, cta) (b, ctb) -> (firstP a cta, seconP b ctb)) augVals $ drop 1 augVals where
--  augVals = (1, L): sortBy compFunc vals++[(4000, G)]
--  firstP a L = a
--  firstP a G = a+1 
--  seconP b L = b-1
--  seconP b G = b
--  compFunc (a, cta) (b, ctb) =  if a==b then compare cta ctb else compare a b 
--
--genIntervalsFromWF :: WfMap -> [(Property, [(Int, Int)])]
--genIntervalsFromWF wfMap = [(prop, genIntervals valData) | (prop, valData) <- M.assocs $ valsMap wfMap  ]
---- 138854747510372
---- 138942983952898
--genRepresentativeParts :: WfMap -> [(Part, (Interval, Interval, Interval, Interval))]
--genRepresentativeParts wfMap =  do 
--  xI <- traceWInfo "x intervals = " $ genIntervals $ fromJust $ M.lookup X vMap
--  mI <- traceWInfo "m intervals = " $ genIntervals $ fromJust $ M.lookup M vMap
--  aI <- traceWInfo "a intervals = " $ genIntervals $ fromJust $ M.lookup A vMap
--  sI <- traceWInfo "s intervals = " $ genIntervals $ fromJust $ M.lookup S vMap 
--  return  (Part {x = snd xI, m =snd mI, a = snd aI, s = snd sI}, (xI, mI, sI, aI))  where
--  --return  (Part {x = x1, m = m1, a = a1, s = s1}, (x2-x1+1)*(m2-m1+1)*(a2-a1+1)*(s2-s1+1)) where
--  vMap = valsMap wfMap
---- 16869,2789651532057)
--valsMap :: WfMap -> M.Map Property [(Int, CompType)] 
--valsMap wfMap = foldl update initMap allRules where
--  initMap = M.fromList [(prop, []) | prop <- [X, M, A, S]]
--  allRules = concat $ M.foldl' (\ls wf -> (fullRules wf):ls) [] wfMap
--  update propMap FullRule{value = val, compType = ct, prop = prop} =
--     M.adjust ((val, ct):) prop propMap  

--solution2 :: String -> Int
--solution2 file = foldl' 
--  (\acc (_, ((x1, x2), (m1,m2), (a1, a2), (s1, s2) )) -> acc + (x2-x1+1)*(m2-m1+1)*(a2-a1+1)*(s2-s1+1)) 0 
--  $ filter ((isAccepted' wfMap "in") .fst) parts where
--  (wfMap, _) = parseFile file
--  parts = genRepresentativeParts wfMap

-- path :: WfMap -> String -> Part -> [String]
-- path wfMap initWF part =  takeWhile (\wfName -> wfName /= "R" ) $ 
--               iterate 
--               (\name -> case M.lookup name wfMap  of 
--                 Just wf -> nextWorkFlow wf part
--                 Nothing -> "R")
--               initWF
              

--path :: Part -> 
--path part = takeWhile (/= Nothing) $ iterate (\str -> str >>= (\s -> (`nextWorkFlow` part)   <$>  M.lookup s wfs)) $ Just "in"
--path :: Part -> WfMap -> [Maybe String]
--path ::  WfMap -> String -> Part -> [Maybe String]
--path wfMap initWF part = takeWhile (/= Nothing) $ iterate (>>= (\name -> (`nextWorkFlow` part)   <$>  M.lookup name wfMap)) $ Just initWF

--isAccepted :: WfMap -> Part -> Bool
--isAccepted wfmap part = (last $ path wfmap "in" part ) == Just "A"  



