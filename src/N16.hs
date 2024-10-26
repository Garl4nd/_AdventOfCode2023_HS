module N16 
(getSolutions16) where

import Control.Monad.ST 
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import GHC.Arr (freezeSTArray, unsafeFreezeSTArray)
import Data.Array.ST
import qualified Data.Array as A
import Data.Array ((!))
--import qualified Data.Sequence as S
--import Data.Sequence ((><), (|>), (<|), Seq ((:|>)), Seq ((:<|)))
import Control.Monad (forM_, unless)
import GHC.Arr (newSTArray, boundsSTArray)
import Debug.Trace
import qualified Data.Foldable as A
import Data.List (intercalate)
import ParsingFuncs (countIf)
type Position = (Int, Int)
data Direction = L | R | U | D deriving (Eq, Show)
data Beam  =  Beam {pos :: Position, dir :: Direction} deriving (Eq, Show)
--data Beam = Beam {xyd :: PhaseCoords}  
--type BeamSeq = S.Seq Beam
type BeamSeq = [Beam]
type BoolGrid = A.Array (Int, Int) Bool
type Tile = Char
type TileGrid = A.Array (Int, Int) Tile
type GameState s = (ST s (STArray s (Int, Int) Bool), BeamSeq, BeamSeq)

traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x = x-- trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x

--traceWInfo2 :: Show a => [Char] -> a -> b -> b
--traceWInfo2 infoStr x passThrough = passThrough-- trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) passThrough

move ::  Beam -> Beam
move (Beam (y,x) dir)  = Beam {pos = newpos, dir = dir} where
  newpos = case dir of
    U -> (y-1, x)
    D -> (y+1, x)
    L -> (y, x-1)
    R -> (y, x+1)  
  
isVertical :: Beam -> Bool 
isVertical Beam {dir = bdir} = (bdir == U || bdir == D)

isHorizontal :: Beam -> Bool 
isHorizontal Beam {dir = bdir} = (bdir == L || bdir == R)

flipBeam :: Beam  -> Beam 
flipBeam beam = beam {dir = newDir} where 
  newDir = case dir beam of 
    U -> D 
    D -> U
    L -> R
    R -> L
    
reflect :: Beam -> Tile -> Beam
reflect beam '/'  =  beam {dir = newDir} where 
  newDir = case dir beam of 
    L -> D
    R -> U
    D -> L
    U -> R 
reflect beam '\\' = flipBeam (reflect beam '/') 
reflect beam _ = beam


updateBeamSeq :: TileGrid -> BeamSeq -> BeamSeq -> (BeamSeq, BeamSeq)
updateBeamSeq _ [] initBeams = ([], initBeams)
updateBeamSeq layoutAr (beam:rest) initBeams
  | y <ymin || y> ymax || x< xmin || x > xmax || beam `elem` initBeams = updateWAr rest initBeams     
  | currentTile == '|' = let uBeam = beam {dir = U}
                             dBeam = beam {dir = D}
                             (updatedBeams, updatedInits) = updateWAr rest initBeams in
                                                        if (isHorizontal beam) then  (uBeam:dBeam: updatedBeams,uBeam:dBeam: updatedInits) 
                                                         else (beam:updatedBeams, initBeams)
  | currentTile == '-' = let lBeam = beam {dir = L}
                             rBeam = beam {dir = R}
                             (updatedBeams, updatedInits) = updateWAr rest initBeams in
                                                        if (isVertical beam) then  (lBeam:rBeam: updatedBeams,lBeam:rBeam: updatedInits) 
                                                         else (beam:updatedBeams, initBeams)
  | otherwise =  let (updatedBeams, updatedInits) = updateWAr rest initBeams in
                 ((reflect beam currentTile) : updatedBeams, updatedInits) where
  currentTile = layoutAr ! (y,x)
  (y, x) =  pos beam
  ((ymin, xmin), (ymax, xmax)) = A.bounds layoutAr
  updateWAr = updateBeamSeq layoutAr

inBounds :: ((Int, Int), (Int, Int)) -> Int -> Int -> Bool
inBounds ((ymin, xmin), (ymax, xmax)) y x =  y >=ymin && y<= ymax && x>= xmin && x <= xmax   

updateBeamSeq2 :: TileGrid -> BeamSeq -> BeamSeq -> (BeamSeq, BeamSeq)
updateBeamSeq2 layoutAr beams initBeams = foldr updateLists ([], initBeams) beams where
  updateLists beam acc   
    | (not $ inBounds bounds y x) || beam `elem` initBeams =  acc     
    | currentTile == '|' &&  isHorizontal beam = let uBeam = beam {dir = U}
                                                     dBeam = beam {dir = D}
                                                     in (uBeam:dBeam:(fst acc), beam:(snd acc))
    | currentTile == '-' && isVertical beam = let lBeam = beam {dir = L}
                                                  rBeam = beam {dir = R}
                                                  in (lBeam:rBeam:(fst acc), beam:(snd acc))
    | otherwise = ((reflect beam currentTile) : (fst acc), snd acc) where
      
      currentTile =  layoutAr ! (y,x)
      (y, x) =  pos beam
      bounds = A.bounds layoutAr
     
update :: TileGrid -> State (GameState s) ()
update tileGrid = do
  (stsAr, currentBeams, initBeams) <- get
--  let (updatedBeams, updatedInits) = updateBeamSeq2 tileGrid currentBeams initBeams
--  let movedBeams = filter (\Beam {pos = (y,x)} -> inBounds bounds y x) $ move <$> updatedBeams
  let movedBeams = move <$> currentBeams
  let (updatedBeams, updatedInits) = updateBeamSeq2 tileGrid movedBeams initBeams 
  let updatedAr = do 
              stAr <- stsAr
              forM_ updatedBeams $ \Beam {pos = bpos} -> writeArray stAr bpos True
              return stAr
  put (updatedAr, updatedBeams, updatedInits) where
    bounds = A.bounds tileGrid
  
simulation :: TileGrid -> State (GameState s) ()
simulation tileGrid = do 
  (_, currentBeams, _ ) <- get
  unless (null (traceWInfo "Current beams" currentBeams) ) $ do
	  update tileGrid
  	  simulation tileGrid

-- updateBeamSeqR :: TileGrid -> BeamSeq -> BeamSeq -> (BeamSeq, BeamSeq)
-- updateBeamSeqR layoutAr beams initBeams = foldr updateLists ([], initBeams) beams where
--   updateLists beam acc   
--     | (not $ inBounds bounds y x) || beam `elem` initBeams =  acc     
--     | currentTile == '|' &&  isHorizontal beam = let uBeam = beam {dir = U}
--                                                      dBeam = beam {dir = D}
--                                                      in (uBeam:dBeam:(fst acc), beam:(snd acc))
--     | currentTile == '-' && isVertical beam = let lBeam = beam {dir = L}
--                                                   rBeam = beam {dir = R}
--                                                   in (lBeam:rBeam:(fst acc), beam:(snd acc))
--     | otherwise = ((reflect beam currentTile) : (fst acc), snd acc) where
      
--       currentTile = traceWInfo "currentTile" $ layoutAr ! (y,x)
--       (y, x) =  pos beam
--       bounds = A.bounds layoutAr
updateBeamSeqR ::  BeamSeq -> BeamSeq -> ReaderT TileGrid (State (GameState s)) (BeamSeq, BeamSeq)
updateBeamSeqR beams initBeams = do
  tileGrid <- ask
  return $ foldr (updateLists tileGrid) ([], initBeams) beams where
  updateLists tileGrid beam acc   
    | (not $ inBounds bounds y x) || beam `elem` initBeams =  acc     
    | currentTile == '|' &&  isHorizontal beam = let uBeam = beam {dir = U}
                                                     dBeam = beam {dir = D}
                                                     in (uBeam:dBeam:(fst acc), beam:(snd acc))
    | currentTile == '-' && isVertical beam = let lBeam = beam {dir = L}
                                                  rBeam = beam {dir = R}
                                                  in (lBeam:rBeam:(fst acc), beam:(snd acc))
    | otherwise = ((reflect beam currentTile) : (fst acc), snd acc) where
      
      currentTile =  tileGrid ! (y,x)
      (y, x) =  pos beam
      bounds = A.bounds tileGrid

updateR ::  ReaderT TileGrid (State (GameState s)) ()
updateR = do
  (stsAr, currentBeams, initBeams) <- lift get
  let movedBeams = move <$> currentBeams
  (updatedBeams, updatedInits) <- updateBeamSeqR  movedBeams initBeams 
  let updatedAr = do 
              stAr <- stsAr
              forM_ updatedBeams $ \Beam {pos = bpos} -> writeArray stAr bpos True
              return stAr
  lift $ put (updatedAr, updatedBeams, updatedInits) 
  
simulationR :: ReaderT TileGrid (State (GameState s) )  ()
simulationR  = do
  (_, currentBeams, _ ) <- lift $ get
  unless (null (traceWInfo "Current beams" currentBeams) ) $ do 
    updateR 
    simulationR


gameRes :: TileGrid -> Beam -> BoolGrid
gameRes ar initBeam  =   let (stAr, _, _) =  execState (simulation ar)  (newSTArray (A.bounds ar) False, [initBeam], [initBeam]) in
                runSTArray stAr

gameResR :: TileGrid -> Beam -> BoolGrid
gameResR tileGrid initBeam  = runSTArray $ stAr where 
                                  state = runReaderT  simulationR tileGrid                                                           
                                  (stAr, _, _) =  execState state  (newSTArray (A.bounds tileGrid) False, [initBeam], [initBeam])
  

--firstEl :: S.Seq a -> a
--firstEl (x :<| rest ) = x
--traceBeams :: Beam -> BoolGrid
--traceBeams initBeam = let initBeamSeq = S.singleton initBeam 
                          
  --                                                          in _
                          
--deleteAt :: Int -> S.Seq a-> S.Seq a
--deleteAt idx seq = S.take idx seq >< S.drop (idx+1) seq

parseFile :: String -> TileGrid
parseFile file = let h = length lns 
                     w = length $ head lns
                     lns = lines file in
                  A.listArray ((1,1), (h, w)) $ concat lns 

charGridToStr :: TileGrid -> [String]
charGridToStr charGrid = let rows = [[charGrid ! (y, x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                             ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid in show <$> rows   

boolGridToStr :: BoolGrid -> [String]
boolGridToStr boolGrid =  charGridToStr $ (\el -> if el then '#' else '.') <$> boolGrid

solutionAr :: String -> BoolGrid
solutionAr file =  gameResR  (parseFile file) Beam {pos = (1, 0), dir = R}

countVisits :: String -> Beam -> Int
countVisits file initBeam = countIf id $ A.toList  (gameResR (parseFile file) initBeam)
 
solution1 :: String -> Int 
solution1 file = countVisits file  Beam {pos = (1, 0), dir = R}

solution2 :: String -> Int
solution2 file = maximum $ (countVisits file) <$> initBeams where
  initBeams = upper ++ lower ++ left ++ right
  left = [Beam (y, xmin-1) R | y <- [ymin.. ymax]]
  right = [Beam (y, xmax+1) L | y <- [ymin..ymax]]
  upper = [Beam (ymin -1, x) D | x <- [xmin.. xmax]]
  lower = [Beam (ymax+1, x) U | x <- [xmin..xmax]]
  ((ymin, xmin), (ymax, xmax)) = A.bounds $ parseFile file

 
getSolutions16 :: String -> IO (Int, Int)
getSolutions16 filename = do
  file <- readFile filename
  --print $ parseFile file
  putStr $ unlines  (charGridToStr $ parseFile file)
  putStr $ unlines  (boolGridToStr $ solutionAr file)
  return (solution1 file , solution2 file )
