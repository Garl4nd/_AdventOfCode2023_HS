module N10
(getSolutions10) where
import ParsingFuncs (pairs)
import Data.Array ((!), (//))
import qualified Data.Array as A
import Data.Maybe (fromJust)
import Data.List (sort, groupBy, sortOn, intercalate)
import Data.Function (on)
data Direction = L | R | U | D deriving (Eq, Show, Ord)
type PipeType = (Direction, Direction)
data Tile = Pipe PipeType | Empty | Mouse deriving (Eq, Show)

type Position = (Int, Int)
data PipeWPos = PipeWPos { directions :: PipeType, pos:: Position}  deriving (Eq, Show)
type TileGrid = A.Array Position Tile
opposite :: Direction -> Direction
opposite L = R
opposite R = L
opposite U = D
opposite D = U

newPos ::  Direction -> Position -> Position
newPos direction (y, x) = case direction of 
    L -> (y, x-1)
    U -> (y-1, x)
    R -> (y, x+1)
    D -> (y+1, x)

charToTile :: Char -> Tile
charToTile c = case c of
    '|' -> Pipe (D, U)
    '-' -> Pipe (L, R)
    'L' -> Pipe (R, U)
    'J' -> Pipe (L, U)
    '7' -> Pipe (L, D)
    'F' -> Pipe (R, D)
    '.' -> Empty
    'S' -> Mouse
    --_ -> Empty

--allPipeTypes :: [PipeType]
--allPipeTypes = [ (D, U),  (L, R), (R, U), (L, U), (L, D), (R, D)]

parsePipeFile :: String -> TileGrid
parsePipeFile file = A.listArray ((1,1) , (length rows, length $ head rows)) $ concat  [charToTile <$> row | row<- rows] where
    rows = lines file

canPass ::  Direction-> Tile -> Bool
canPass moveDirection targetTile = case targetTile of 
    Pipe p ->   opposite moveDirection `isContainedIn` p
    _ -> False

isContainedIn :: Direction -> PipeType -> Bool
isContainedIn direction (d1, d2) = direction == d1 || direction == d2

move :: Position -> Tile -> Direction -> (Position, Direction)
move currentPos tile incomingDirection = case tile of 
    Pipe pipeDirections -> (newPos newDirection currentPos, opposite newDirection) where
        newDirection = if incomingDirection == dir2 then dir1 else dir2
        (dir1, dir2) = pipeDirections
    _  -> (currentPos, incomingDirection)
    
-- currentPos:posHistor
walkFrom :: TileGrid -> Position -> Direction -> [(Position, Direction)]
walkFrom tileGrid initPosition initDirection =   iterate  walk  (initPosition, initDirection) where
    walk :: (Position, Direction) -> (Position, Direction)
    walk  (currentPosition, incomingDirection)  = move currentPosition (tileGrid ! currentPosition) incomingDirection 
--walk TileGrid (currentPos, posHistory) incomingDirection  =  

findMouse :: TileGrid -> Maybe Position
findMouse tileGrid = case filter (\(i,j) -> tileGrid ! (i,j) ==Mouse) (A.indices tileGrid) of 
    pos:rest -> Just pos
    _ -> Nothing

possiblePipes :: TileGrid -> Position -> [PipeType]
possiblePipes tileGrid pos =    [(i,j) | i<- allowedDirections, j<- allowedDirections, i/=j]  where
    allowedDirections = filter (\direction -> canPass direction $ tileGrid ! newPos direction pos) [L, U, R, D]
    
    

mousePath  :: String -> [PipeWPos]
mousePath file =  PipeWPos initPipeType mousePos :[PipeWPos pipetype pos | pos <- path, let Pipe pipetype = tileGrid ! pos] where-- ((\pos -> (pos, let Pipe pipetype = tileGrid! pos in pipetype)) <$> path) where
    path = takeWhile (/= mousePos)  (drop 1 $ fst <$> posHistory ) 
    posHistory = walkFrom (tileGrid // [(mousePos, Pipe initPipeType)])  mousePos initDirection 
    mousePos = fromJust $ findMouse tileGrid
    initPipeType =  head $ possiblePipes tileGrid mousePos    
    initDirection = fst initPipeType
    tileGrid = parsePipeFile file 

solution1 :: String -> Int
solution1 file = div (length $ mousePath file ) 2


writePath :: String -> IO ()
writePath file = do 
    let tileGrid = parsePipeFile file
    let path = mousePath file
    writeFile "outputs/path.txt" $ intercalate "\n" [[if  not $ (y,x)  `elem` (pos <$> path ) then '.' else  ln !! (x-1) |x <- [1..length ln]] | (y, ln) <-  zip [1..] $  lines file ]
    return ()

data Orientation = Oriented Direction | Neutral  deriving (Eq, Show)
data State = State {orientation::Orientation, isInside:: Bool} deriving (Eq, Show)

transition :: State  -> Tile -> State
transition (State orientation inside) tile = case tile of 
    Pipe directions 
        | U `isContainedIn` directions && D `isContainedIn` directions -> State {isInside = not $ inside, orientation = orientation } 
        | not (U `isContainedIn` directions) && not (D `isContainedIn` directions) -> State {isInside = inside, orientation = orientation } 
        | otherwise -> case orientation of 
            Oriented dir ->  State Neutral (if dir `isContainedIn` directions  then inside else not inside )    
            Neutral -> State orientation inside where            
                orientation = Oriented $ if U `isContainedIn` directions then U else D
    _ -> State orientation inside

stateToChar :: State -> Char
stateToChar state
    | isInside state = 'I'
    | not $ isInside state = 'X'

updatedTileGrid :: TileGrid ->TileGrid
updatedTileGrid tileGrid =  tileGrid // [(mousePos, Pipe initPipeType)] where
    maybeMousePos = findMouse tileGrid    
    initPipeType =  head $ possiblePipes tileGrid mousePos
    mousePos = fromJust maybeMousePos 
    
solution2 :: String -> Int
solution2 file =  length insides where
    tileGrid = updatedTileGrid $ parsePipeFile file
    yRows = groupBy  ( (==) `on` fst.fst)  $ A.assocs tileGrid 
    --print yRows
    path = mousePath file
    states row = scanl (\state ((y,x), tile) -> if  not $ (y,x)  `elem` (pos <$> path ) then state else  (transition state tile)) (State Neutral False) row where
        ypos row = fst.fst $ head row        
    insides = filter (\((y,x), state) -> (not $ (y,x)  `elem` (pos <$> path )) && isInside state)   (zip (A.indices tileGrid) (concat $ states <$> yRows) )

solution2' :: String -> Int
solution2' file =  res  where
    tileGrid = updatedTileGrid $ parsePipeFile file
    yRows = groupBy  ( (==) `on` fst.fst)  $ A.assocs tileGrid 
    --print yRows
    path = mousePath file
    stateRow row = scanl (\state ((y,x), tile) -> if  not $ (y,x)  `elem` (pos <$> path ) then state else  (transition state tile)) (State Neutral False) row where
        ypos row = fst.fst $ head row
    stateRows = stateRow <$> yRows
    res = sum$   [ length $ [  x  |(x, state) <-  zip [1..length states] states, (y,x)  `notElem` (pos <$> path ) && isInside state] | (y, states) <-  zip [1..]   stateRows ]
    --res = sum$   [ length $ filter (\(x, state) -> (y,x)  `notElem` (pos <$> path ) && isInside state)  $ zip [1..length states] states | (y, states) <-  zip [1..]   stateRows ]
  
solution2'' :: String -> Int
solution2'' file =  sum $ numInsides <$> yRows where
    tileGrid = updatedTileGrid $ parsePipeFile file
    yRows = groupBy  ( (==) `on` fst.fst)  $ A.assocs tileGrid     
    path = mousePath file
    stateRow row = scanl (\(state,_)  ((y,x), tile) -> if  not $ (y,x)  `elem` (pos <$> yPath ) then (state, isInside state) else  (transition state tile, False)) (State Neutral False, False) row where
        ypos  = fst.fst $ head row
        yPath = filter (\PipeWPos {pos=pos} -> fst pos == ypos) path
    numInsides row= length $ filter snd $ stateRow row

classifyMaze :: String -> IO ()
classifyMaze file = do 
    let tileGrid = updatedTileGrid $ parsePipeFile file
    let yRows = groupBy  ( (==) `on` fst.fst)  $ A.assocs tileGrid 
    --print yRows
    let  path = mousePath file
    let augRow row = scanl (\state ((y,x), tile) -> if  not $ (y,x)  `elem` (pos <$> path ) then state else  (transition state tile)) (State Neutral False) row where
        ypos row = fst.fst $ head row
    let augRows = augRow <$> yRows
    writeFile "outputs/path.txt" $ intercalate "\n" [[if  not $ (y,x)  `elem` (pos <$> path ) then stateToChar (states!! (x-1)) else  ln !! (x-1) |x <- [1..length ln]] | (y, ln, states) <-  zip3 [1..]  (lines file)  augRows ]
    return ()

getSolutions10 :: String -> IO (Int, Int)
getSolutions10 filename = do 
    file <- readFile filename     
    --classifyMaze file 
    return (solution1 file, solution2'' file)