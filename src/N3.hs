module N3
    ( getSolutions3
    ) where
import Data.List
import Data.Char (isNumber)
import qualified Data.Bifunctor

type AugStr  = [(Int, Char)]

augStrToStr :: AugStr -> String
augStrToStr  = foldl (\acc (_, char) -> mappend acc [char]) ""

augSplitOn :: (Char -> Bool) -> String   -> [PosString]
augSplitOn p s  = map strWPos $ splitHelperFunc p (zip [1..] s) where
    splitHelperFunc :: (Char -> Bool) -> AugStr -> [AugStr]
    splitHelperFunc p augStr = let aug p (idx, c) =  p c in
                      case dropWhile (aug p)  augStr of
                        [] -> []
                        s' -> w : splitHelperFunc p s''
                            where (w, s'') = break (aug p) s'

type Pos = (Int, Int)
type PosString = (Pos, String)
type PosLine = [PosString]

strWPos :: AugStr -> PosString
strWPos augS = (( (fst.head) augS, (fst.last) augS), augStrToStr augS)

isAdjacent :: PosString -> PosString -> Bool
isAdjacent sym1 sym2 = let ((start1, end1), _) =  sym1
                           ((start2, end2), _) = sym2 in
                           (start2 `elem` [ (start1-1) .. (end1+1)] || end2 `elem` [(start1-1) .. (end1+1)])

getAllAdjacentOnLine :: PosString -> PosLine -> [PosString]
getAllAdjacentOnLine symbol =  filter ( `isAdjacent`  symbol )

getAllAdjacent :: PosLine -> [PosLine] -> [(PosString, [PosString])]
getAllAdjacent symbols numLines = [ (symbol, mconcat (map (getAllAdjacentOnLine symbol)  numLines)) |symbol <- symbols]

hasAdjacentSymbols :: PosString -> PosLine -> Bool
hasAdjacentSymbols = any.isAdjacent

allWithAdjacent :: [PosString] -> [PosString] -> [PosString]
allWithAdjacent numStrs symStrs = filter (`hasAdjacentSymbols` symStrs) numStrs

allWithAdjacentList :: [PosString] -> [[PosString]] -> [PosString]
allWithAdjacentList numStrs symStrList = filter (\numStr -> or [numStr `hasAdjacentSymbols` symStrs  | symStrs<- symStrList]  ) numStrs

makeNeighbouringList:: [PosLine] -> [[PosLine]]
makeNeighbouringList ls = zipWith3 (\a b c -> [a,b,c]) ls upperRange lowerRange  where
                            upperRange = rotateList 1 ls
                            lowerRange = rotateList (-1) ls                            
                            rotateList n x
                                    | n >= 0 = take (length x) (drop n $ cycle x )
                                    | otherwise = take (length x) (drop (length x +n) $ cycle x )

calc2D :: (PosLine -> [PosLine] -> a) -> [PosLine] -> [PosLine] -> [a]
calc2D f numStrList symStrList = zipWith  f  augNumList adjLinesymbolList where
                            adjLinesymbolList  = makeNeighbouringList augSymList
                            augNumList = []: numStrList ++ [[]]
                            augSymList = []: symStrList ++ [[]]

allWithAdjacent2D :: [PosLine] -> [PosLine] -> [PosLine]
allWithAdjacent2D  = calc2D allWithAdjacentList

allAdjacentNums :: [PosLine] -> [PosLine] -> [[(PosString, [PosString])]]
allAdjacentNums  =  calc2D getAllAdjacent

type AdjList = (String, [Int])
simplifyAdjList :: (PosString, [PosString]) -> AdjList
simplifyAdjList  =  Data.Bifunctor.bimap snd (map (read . snd))

classifyByAdjNum :: [[AdjList]] -> [[AdjList]]
classifyByAdjNum ls = [filter (\(sym, nums) -> length nums == count) (mconcat ls)  | count <- [0..]]

sumOfPosLine :: PosLine -> Int
sumOfPosLine  = sum . map  (read.(filter isNumber.snd))   

gears :: [[AdjList]]  -> [AdjList]
gears ls = filter (\(sym, _) -> sym == "*") (classifyByAdjNum ls !! 2    )

sumOfGearRatios :: [[AdjList]]  -> Int
sumOfGearRatios ls = sum $ map (\(_, [x,y]) -> x*y) $ gears ls

getSolutions3 :: String -> IO (Int, Int)-- [(Int, [PosString])]
getSolutions3 filename = do
    file <- readFile filename
    let numStrs =    augSplitOn (not.isNumber) <$> lines file
        symStrs =   augSplitOn (\c -> c== '.'  ||  isNumber c  ) <$> lines file
        symbolAdjacentNums =   map simplifyAdjList <$> allAdjacentNums  symStrs numStrs        
        adjacentSymbols = allWithAdjacent2D numStrs symStrs
        gearRatios = sumOfGearRatios symbolAdjacentNums     
        solution1 = sum $ map sumOfPosLine adjacentSymbols
        solution2 = gearRatios
    writeFile "outputs/numStrs.txt" $  intercalate "\n" $  show <$> numStrs
    writeFile "outputs/symStrs.txt" $  intercalate "\n" $ show <$> symStrs
    writeFile "outputs/adjacent.txt" $  intercalate "\n" $  show <$> adjacentSymbols
    writeFile "outputs/adjacentToSymbol.txt" $  intercalate "\n" $  show <$>  symbolAdjacentNums
    writeFile "outputs/adjacentToSymbolClassification.txt" $  intercalate "\n" $  show <$>  take 5  (classifyByAdjNum symbolAdjacentNums)
    appendFile "outputs/adjacentToSymbolClassification.txt" $ "\n___\nGears = "<> show  (gears symbolAdjacentNums)
    appendFile "outputs/adjacentToSymbolClassification.txt" $ "\nSum of gear ratios = "<> show  gearRatios
    return (solution1, solution2)
    




