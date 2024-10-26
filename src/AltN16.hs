{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module AltN16 
(getSolutions16Alt) where
  
import Control.Monad.ST 
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import GHC.Arr (freezeSTArray, unsafeFreezeSTArray)
import Data.Array.ST
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.Array ((!))
import Data.Bits
--import qualified Data.Sequence as S
--import Data.Sequence ((><), (|>), (<|), Seq ((:|>)), Seq ((:<|)))
import Control.Monad (forM_, unless, when)
import GHC.Arr (newSTArray, boundsSTArray, STArray (STArray), readSTArray)
import Debug.Trace
import Data.List (intercalate, unfoldr)
import ParsingFuncs (countIf)
import qualified Data.ByteString as B
import qualified Data.Array as ST
type Position = (Int, Int)
data Direction = L | R | U | D deriving (Eq, Show)
data Beam  =  Beam {pos :: Position, dir :: Direction} deriving (Eq, Show)

--data Beam = Beam {xyd :: PhaseCoords}  
--type BeamSeq = S.Seq Beam
type DirC = Int
type BeamSeq = [Beam]
type BoolGrid = A.Array (Int, Int) Bool
type DirCGrid = A.Array (Int, Int) DirC
type Tile = Char
type TileGrid = A.Array (Int, Int) Tile
type GameState s = ST s (STArray s (Int, Int) DirC, BeamSeq) 

traceWInfo :: Show a => [Char] -> a -> a
traceWInfo infoStr x = x --trace ("\n** "++infoStr ++" "++ show x ++"**\n" ) x


encodeD :: Direction -> DirC
encodeD U = bit 0
encodeD D = bit 1
encodeD L = bit 2
encodeD R = bit 3

combineD :: Direction -> DirC -> DirC
combineD dir dirC = encodeD dir .|. dirC

dirCToChar :: DirC -> Char
dirCToChar 0 = '.'
dirCToChar dirC
  | dirC == encodeD U = '^'
  | dirC == encodeD D = 'v'
  | dirC == encodeD L = '<'
  | dirC == encodeD R = '>'
  | otherwise = case  nonZeroBits dirC of
		  2 -> '2'
		  3 -> '3'
		  4 -> '4'
		  _ -> 'm'
move ::  Beam -> Beam
move (Beam (y,x) dir)  = Beam {pos = newpos, dir = dir} where
  newpos = case dir of
    U -> (y-1, x)
    D -> (y+1, x)
    L -> (y, x-1)
    R -> (y, x+1) 

nonZeroBits :: Int -> Int
nonZeroBits n = sum $ unfoldr (\x -> if x <= 0 then Nothing else Just (x .&. 1, shiftR x 1)) n

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


inBounds :: ((Int, Int), (Int, Int)) -> Int -> Int -> Bool
inBounds ((ymin, xmin), (ymax, xmax)) y x =  y >=ymin && y<= ymax && x>= xmin && x <= xmax   
 
updateBeamSeqST :: TileGrid-> GameState s -> ST s BeamSeq 
updateBeamSeqST tileGrid st = do 
              (dirAr, beams ) <- st
              ar <- freezeSTArray dirAr
  --let currentDir  = runSTArray  $ do {ar<- dirGrid; unsafeFreezeSTArray ar}--runST $ (freezeSTArray <$> dirGrid) --do {ar <- dirGrid;return $ freeze ar}--readArray ar (1,1)}    
              let movedBeams = move <$> beams
              let newBeams =  foldr updateLists [] movedBeams
                  updateLists beam acc   
                         | (not $ inBounds bounds y x) || (ar ! (y,x)  .&. (encodeD $ dir beam)) /= 0  =  acc     
                         | currentTile == '|' &&  isHorizontal beam = let uBeam = beam {dir = U}
                                                                          dBeam = beam {dir = D}
                                                      in uBeam:dBeam: acc
                         | currentTile == '-' && isVertical beam = let lBeam = beam {dir = L}
                                                                       rBeam = beam {dir = R}
                                                    in lBeam:rBeam: acc
                         | otherwise = (reflect beam currentTile) :acc where
        
                          currentTile =  tileGrid ! (y,x)
                          (y, x) =  pos  beam
                          bounds = A.bounds tileGrid
              return newBeams


updateST ::  TileGrid -> GameState s -> GameState s
updateST tileGrid st = do
  updatedBeams <- updateBeamSeqST tileGrid st 
  stAr <- fst <$> st
  forM_ updatedBeams $ \Beam {pos = bpos, dir = bdir} ->  modifyArray stAr bpos (combineD bdir) 
  return (stAr, updatedBeams)  where
    bounds = A.bounds tileGrid


updateST' ::  TileGrid -> GameState s -> GameState s
updateST' tileGrid st = do
  (stAr, beams) <- st   
  forM_ beams $ \Beam {pos = (y,x), dir = bdir} -> when (inBounds bounds y x) $  modifyArray stAr (y,x) (combineD bdir) 
  updatedBeams <- updateBeamSeqST tileGrid st
  return (stAr, updatedBeams) where
    bounds = A.bounds tileGrid

simulationR :: ReaderT TileGrid (State (GameState s) )  ()
simulationR  = do
  st <- lift  get
  tileGrid <- ask
  let sim st' = do 
        (ar, beams) <- updateST' tileGrid st' 
        if null (traceWInfo "current beams" beams) then return (ar, []) else sim (return  (ar, beams)) 
  lift $ put (sim st)

gameResR :: TileGrid -> Beam -> DirCGrid
gameResR tileGrid initBeam  =  runSTArray $ fst <$> st where 
                                  state = runReaderT  simulationR tileGrid                                                           
                                  st =  execState state  $ do
                                    ar <- newSTArray (A.bounds tileGrid) 0
                                    return (ar, [initBeam])

parseFile :: String -> TileGrid
parseFile file = let h = length lns 
                     w = length $ head lns
                     lns = lines file in
                  A.listArray ((1,1), (h, w)) $ concat lns 

charGridToStr :: TileGrid -> [String]
charGridToStr charGrid = let rows = [[charGrid ! (y, x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                             ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid in 
                             show <$> rows   

boolGridToStr :: BoolGrid -> [String]
boolGridToStr boolGrid =  charGridToStr $ (\el -> if el then '#' else '.') <$> boolGrid

dirGridToStr :: DirCGrid -> [String]
dirGridToStr dirGrid =  charGridToStr $ dirCToChar <$> dirGrid



solutionAr :: String -> DirCGrid
solutionAr file =  gameResR  (parseFile file) Beam {pos = (1, 0), dir = R}

countVisits :: String -> Beam -> Int
countVisits file initBeam = countIf (/=0) $ toList  (gameResR (parseFile file) initBeam)
 
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

 
  


getSolutions16Alt :: String -> IO (Int, Int)
getSolutions16Alt filename = do
  file <- readFile filename
  --print $ parseFile file
  putStr $ unlines  (charGridToStr $ parseFile file)
  putStr $ unlines  (dirGridToStr $ solutionAr file)
  putStr  "begin\n"
  return (solution1 file ,0)--solution2 file )
