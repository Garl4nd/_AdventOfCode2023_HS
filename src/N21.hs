module N21  
    (getSolutions21) where

import qualified Data.Array.Unboxed as A
import Data.Array ((!), (//))
import qualified Data.Set as S
import Data.Function.Memoize (memoize, memoize2, memoFix2, memoFix3, memoFix)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.List (nub)



type Position = (Int, Int)
type Move = (Int, Int)
type CharGrid = A.Array Position Char
type PositionSet = S.Set Position
type Memo f = f -> f

move ::  Position -> Move -> Position
move (y,x) (dY, dX)  = (y + dY, x + dX)

-- inefficient : recursively find solution for each visited tile getMoves :: CharGrid -> Position -> Int -> PositionSet
-- getMoves grid = getMovesInnerM where
--     getMovesInnerM = memoFix2 getMovesInner
--     getMovesInner :: Memo (Position -> Int -> PositionSet)
--     getMovesInner _  _ 0 = S.empty 
--     getMovesInner _ (y,x) 1 = S.fromList  $ filter (\pos' -> A.inRange (A.bounds grid) pos' && grid ! pos' /= '#')  [(y+1, x), (y-1,x), (y, x-1), (y, x+1)]
--     --getMovesInner _ (y,x) 2 = S.fromList  $ filter (\pos' -> A.inRange (A.bounds grid) pos' && grid ! pos' /= '#')  [(y+2, x), (y+1, x-1), (y+1, x+1), (y,x-2), (y,x), (y, x+2), (y-1,x-1), (y-1, x+1), (y-2, x)]
--     --getMovesInner getMovesInner pos n = let nHalfPosSet = getMovesInner pos (n `div` 2) in S.unions $ S.map (\pos' -> getMovesInner pos' (n `div` 2) ) nHalfPosSet 
--     getMovesInner getMovesInner' pos n = let nHalfPosSet = getMovesInner' pos 1 in S.unions $ S.map (\pos' -> getMovesInner' pos' (n -1) ) nHalfPosSet 
--     --getMovesInner getMovesInner' pos n = let nHalfPosSet = getMovesInner' pos 2 in S.unions $ S.map (\pos' -> getMovesInner' pos' (n -2) ) nHalfPosSet 

getMoves :: CharGrid -> Position -> Int -> PositionSet
getMoves grid initPos n = expandSet initSet initSet initN where
    initN = if even n then n else n-1
    initSet = if even n then S.singleton initPos  else S.fromList $ oneOfSet  initPos
    expandSet :: PositionSet -> PositionSet -> Int -> PositionSet 
    expandSet currentSet _ 0 = currentSet 
    expandSet currentSet currentBoundary n' = let 
        newBoundary = S.unions $ S.map (S.filter (`S.notMember` currentSet).newPositions) currentBoundary        
         in expandSet (S.union  currentSet newBoundary) newBoundary (n'-2)
    newPositions =  twoOfSet 
    twoOfSet pos = S.fromList $ concat [oneOfSet pos' | pos' <- oneOfSet pos]
    oneOfSet (y,x) = filter (\pos' -> A.inRange (A.bounds grid) pos' &&  grid !  pos'  /= '#')  [(y+1, x), (y-1,x), (y, x-1), (y, x+1)] 

getMovesInf :: CharGrid -> Position -> Int -> PositionSet
getMovesInf grid initPos n = expandSet initSet initSet initN where
    initN = if even n then n else n-1
    initSet = if even n then S.singleton initPos  else S.fromList $ oneOfSet  initPos
    expandSet :: PositionSet -> PositionSet -> Int -> PositionSet 
    expandSet currentSet _ 0 = currentSet --S.union currentSet currentBoundary
    expandSet currentSet currentBoundary n' = let 
        newBoundary = S.unions $ S.map (\pos' -> S.filter (`S.notMember` currentSet) $ newPositions pos') currentBoundary        
         in expandSet (S.union  currentSet newBoundary) newBoundary (n'-2)
    newPositions =  twoOfSet -- ::  Position -> [Position]
    twoOfSet pos = S.fromList $ concat [oneOfSet pos' | pos' <- oneOfSet pos]--S.fromList $ filter (\pos' -> grid ! mod2 pos' bounds /= '#' && isPath (y,x)  pos')  [(y+2, x), (y+1, x-1), (y+1, x+1), (y,x-2), (y, x+2), (y-1,x-1), (y-1, x+1), (y-2, x)]
    oneOfSet (y,x) = filter (\pos' ->  grid !  mod2 pos' bounds /= '#')  [(y+1, x), (y-1,x), (y, x-1), (y, x+1)] 
    mod' n t = (n+ t) `mod` (2*t+1) - t --if n<=t then n else -(2*t)+n -- x' = if x<= w then x else -(2*w) + x 
    mod2 (m,n) (t,s) = (mod' m t, mod' n s)
    ((_, _), bounds) = A.bounds grid

printMoves :: String  -> Int -> IO (CharGrid, PositionSet)
printMoves gridFile n = let     (grid, initPos) = parseFile gridFile                                
                                moves = getMoves grid initPos n
                                moveGrid = S.toList moves
                                ar  = grid // [(pos', 'O') | pos' <- moveGrid]  // [(initPos, 'S')]
                        in do 
                            putStr $ unlines  (charGridToStr ar)
                            return (ar, moves)

naiveSolution :: CharGrid -> Int -> Int
naiveSolution grid steps = sum $ [pSums ! (y,x) | y <- [0..2*h], x <- [0..2*w], abs y + abs x == steps ] where
    pSums =  partSums grid (if odd steps then odd else even)
    ((_,_), (h,w)) = A.bounds grid
    partSums :: CharGrid -> (Int -> Bool) -> A.Array (Int, Int) Int
    partSums grid parityCheck = A.array ((0,0),(2*h, 2*w)) $ concat [ zip [(y,x) | x<- [0..2*w]] (freePos y)  | y<- [0..2*h]]  where
        ((_, _), (h, w)) = A.bounds grid
        freePos :: Int -> [Int]
        freePos y = tail $ scanl (\acc x -> if parityCheck (y+x) then let 
                                                                x' = if x<= w then x else -(2*w) + x 
                                                                y' = if y<= h then y else -(2*h) + y  
                                                                coords = nub [(y', x'), (y', -x'), (-y', x'), (-y', -x')] in
                                                        acc + sum (cEval.(grid !) <$> coords) else acc) 0 [0..(2*w)]
    cEval '#' = 0 
    cEval _ = 1

parseFile :: String -> (CharGrid, Position)
parseFile file  = let ar = strToCharGrid file                      
                      sPos = fst $ fromJust $ find ((=='S').snd) $ A.assocs ar
                  in (ar, sPos)

strToCharGrid :: String -> CharGrid
strToCharGrid file = A.listArray ((- (numLines `div` 2), -(lineSize `div` 2)), (numLines `div` 2, lineSize `div` 2)) $ concat ls  where
  ls = lines file
  numLines = length ls
  lineSize = length $ head ls

charGridToStr :: CharGrid -> [String]
charGridToStr charGrid = let rows = [[charGrid ! (y, x) | x <- [xmin..xmax] ]| y<- [ymin..ymax]]
                             ((ymin, xmin), (ymax, xmax)) = A.bounds charGrid in show <$> rows   

solution1 :: String -> Int -> Int 
solution1 file steps = let 
                    (grid, initPos) = parseFile file                                
                    moveSet = getMoves grid initPos steps
                in S.size moveSet

solution2 :: String -> Int -> Int 
solution2 file steps = let 
                    (grid, initPos) = parseFile file                                
                    moveSet = getMovesInf grid initPos steps
                in S.size moveSet

printSolution :: String -> Int -> IO Int
printSolution fileName steps  = do 
    file <- readFile fileName    
    (ar, pos) <- printMoves file steps
    let unreached =  filter (\((y,x), c) -> (c=='.') && (abs x + abs y) `mod` 2 == steps `mod` 2 && (abs x + abs y <= steps)   ) $ A.assocs ar
    print $ "unreached: " <> show unreached 
    writeFile "outputs/21_steps.txt" $ unlines  (charGridToStr ar) 
    return $ S.size pos

getSolutions21 :: String -> IO (Int,Int)
getSolutions21 filename = do 
    file <- readFile filename   
    let steps = [65, 65+131, 65+2*131]
        solsForSteps = solution2 file <$> steps
        sol = 617361073602319 -- fit quadratic formula for  (n, solsForSteps(65+n*131)), calculate solution at n = 202300
    return (solution1 file 64, sol)