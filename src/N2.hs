module N2
    ( getSolutions2
    ) where
import Data.List
import qualified Data.Map as M
--import Control.Applicative (Alternative(empty))
testLine = "Game 1: 4 green, 7 blue; 2 blue, 4 red; 5 blue, 2 green, 2 red; 1 green, 3 red, 9 blue; 3 green, 9 blue; 7 green, 2 blue, 2 red"
data Color = Green | Blue | Red deriving (Show, Eq, Ord)
type Balls = (Int, Color)
type BallSet = [Balls]
type BallSetMap = M.Map Color Int
data Game = Game Int [BallSet] deriving Show
parseLine :: String -> Game 
parseLine line = Game 0 [[(4, Green)]]

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

splitOn :: Char -> String -> [String]
splitOn c = wordsWhen (== c)

getGameId :: String -> Int
getGameId line = let [_, intStr] =  splitOn ' ' (head (splitOn ':' line))
                 in read intStr

getBallSetStrings :: String -> [String]                
getBallSetStrings line = splitOn ';' (last (splitOn ':' line))                

strToColor :: String -> Color
strToColor str = case str of 
     "blue" -> Blue
     "red" -> Red
     "green" -> Green 
     _ -> Blue

strToBalls :: String -> Balls
strToBalls ballStr = let [numStr, colorStr] = splitOn ' ' ballStr in (read numStr, strToColor colorStr)

strToBallSet :: String -> BallSet
strToBallSet str=  map strToBalls $ splitOn ',' str
                                
getBallSets :: String -> [BallSet]
getBallSets line = map strToBallSet $ getBallSetStrings line 

getGameFromLine :: String -> Game 
getGameFromLine line = Game (getGameId line) (getBallSets line)

updateMap:: M.Map Color Int -> Balls -> M.Map Color Int 
updateMap current (num, color) = M.insert color num current

emptyMap :: M.Map Color Int
emptyMap = M.fromList [(Blue, 0), (Green,0 ), (Red, 0)]  

ballMapFromBallSet :: BallSet -> BallSetMap
ballMapFromBallSet = foldl updateMap emptyMap

maxValMap :: [M.Map Color Int] -> M.Map Color Int
maxValMap  = foldl (M.unionWith max) emptyMap

power :: M.Map Color Int -> Int
power m = foldl (\ res (c, v) -> res*v ) 1 $ M.toList m

maxValFromStr :: String -> M.Map Color Int
maxValFromStr line = maxValMap $ map ballMapFromBallSet (getBallSets  line)

linePower ::String -> Int
linePower  = power.maxValFromStr

findSolution2 :: String -> IO Int
findSolution2 filename = do
    file <- readFile filename
    let maxValMaps = map maxValFromStr (lines file)
    let powers = map power maxValMaps
    --let frstPow = head powers
    let lineTxt = intercalate "\n" (zipWith (\line maxVal -> line<>": "<>show maxVal <>" -> "<> show (power maxVal) ) (lines file)  maxValMaps)
    writeFile  "outputs/powers.txt" lineTxt
    return $ sum powers

getAllGames :: String -> IO [Game]
getAllGames fileName = do 
    file <- readFile fileName 
    return $ map getGameFromLine (lines file)

ballLimit :: [(Int, Color)]
ballLimit =  [(12, Red), (13, Green), (14, Blue)]

classifyGames :: String -> IO ( [Game], [Game])
classifyGames fileName = do 
    games <- getAllGames fileName
    let (validGames, invalidGames) = partition (isGamePossible ballLimit) games 
        in return ( validGames, invalidGames)

findSolution1 :: String -> IO Int
findSolution1 fileName = do 
    res <- classifyGames fileName    
    let solution = sum (map (\(Game iD _) -> iD )  $ fst res)
    writeFile ("outputs/fileName"<>"_class.txt" ) $ "Valid games:\n" <> intercalate "\n" ( map show $ fst res)
    appendFile ("outputs/fileName"<>"_class.txt" ) $ "\n___\nInvalid games:\n" <> intercalate "\n" ( map show $ snd res)
    appendFile ("outputs/fileName"<>"_class.txt") $ "\n____\nSum of Ids = "<> show  solution
    return solution

getSolutions2 :: String -> IO (Int, Int)
getSolutions2 filename = do 
    s1 <- findSolution1 filename
    s2 <- findSolution2 filename
    return (s1, s2)

isBallSetPossible :: BallSet -> BallSet -> Bool
isBallSetPossible ballLimit ballset  = and  [areBallsAllowed maxBalls balls | maxBalls <- ballLimit, balls <- ballset ] 

isGamePossible :: BallSet -> Game  -> Bool
isGamePossible ballLimit (Game id ballsets)  = all (isBallSetPossible ballLimit) ballsets

areBallsAllowed :: Balls -> Balls -> Bool
areBallsAllowed  (maxBalls, maxColor)  (actualBalls, actualColor)
    | maxColor /= actualColor = True
    | otherwise = actualBalls <= maxBalls



