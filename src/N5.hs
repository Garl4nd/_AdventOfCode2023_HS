module N5
  ( getSolutions5
  ) where

import Control.Arrow ((>>>))
import qualified Data.Set as S
import ParsingFuncs (readStrList, splitBySubstr, splitOn)

data MapEntry = MapEntry
  { seedId :: Int
  , targetId :: Int
  , extent :: Int
  } deriving (Show)

data SeedMap = SeedMap
  { typeString :: String
  , entries :: RangeMap
  } deriving (Show)

newtype RangeMap =
  RangeMap [MapEntry]
  deriving (Show)

type DiscPoints = S.Set Int

parseFile :: String -> ([Int], [SeedMap])
parseFile file = (parseSeeds $ firstLine, parseSeedMaps <$> seedMaps)
  where
    firstLine:seedMaps = splitBySubstr "\n\n" file
    parseSeeds line =
      let [_, seedsStr] = splitOn ':' line
       in readStrList ' ' seedsStr
    parseSeedMaps seedMap =
      SeedMap
        { typeString = head smLines
        , entries = RangeMap (readEntry <$> tail smLines)
        }
      where
        smLines = lines seedMap
        readEntry line =
          case readStrList ' ' line of
            [tId, sId, ext] -> MapEntry sId tId ext

sourceToTarget :: RangeMap -> Int -> Int
sourceToTarget (RangeMap entries) idx =
  case filter (idx `inJumpRegion`) entries of
    entry:_ -> targetId entry + idx - seedId entry
    [] -> idx

inJumpRegion :: Int -> MapEntry -> Bool
x `inJumpRegion` MapEntry {seedId = seedId, extent = extent} =
  x >= seedId && x < seedId + extent

discpoints :: RangeMap -> DiscPoints
discpoints (RangeMap entries) =
  foldl
    S.union
    S.empty
    [ S.fromAscList [seedId entry, seedId entry + extent entry]
    | entry <- entries
    ]
  --foldl S.union S.empty [S.insert  (seedId entry) (S.singleton (seedId entry + extent entry)) | entry <- entries]

preimages :: RangeMap -> Int -> [Int]
preimages (RangeMap entries) y =
  (if y == (sourceToTarget (RangeMap entries) y)
     then [y]
     else [])
    ++ [ x
       | entry <- entries
       , let x = y + seedId entry - targetId entry
       , x `inJumpRegion` entry
       ]

combinedDiscPoints :: RangeMap -> RangeMap -> DiscPoints
combinedDiscPoints sourceMap targetMap =
  S.union
    (discpoints sourceMap)
    (S.fromList $ concatMap (preimages sourceMap) (discpoints targetMap))

instance Semigroup RangeMap where
  rm1 <> rm2 =
    RangeMap $ filter (\(MapEntry sId tId ext) -> sId /= tId && ext > 0) res
    where
      discPointList = S.toList $ combinedDiscPoints rm1 rm2
      res =
        zipWith
          (\x1 x2 -> MapEntry x1 (f x1) (x2 - x1))
          discPointList
          (tail discPointList)
      f = sourceToTarget rm2 . sourceToTarget rm1

instance Semigroup SeedMap where
  SeedMap ts1 entries1 <> SeedMap ts2 entries2 =
    SeedMap (ts1 <> "->" <> ts2) $ entries1 <> entries2

instance Monoid RangeMap where
  mempty = RangeMap []

instance Monoid SeedMap where
  mempty = SeedMap "" mempty

minimumForRange :: SeedMap -> Int -> Int -> Int
minimumForRange seedMap x0 x1 = minimum (f <$> S.toList decisivePoints)
  where
    f = sourceToTarget $ entries seedMap
    decisivePoints =
      S.insert x0 (S.filter (between x0 x1) (discpoints $ entries seedMap))
    between x0 x1 x = x0 <= x && x <= x1

getAllLocations :: String -> [Int]
getAllLocations file = map seedToLocation initSeeds
  where
    (initSeeds, seedMaps) = parseFile file
    seedToLocation = foldl (>>>) id funclist
    funclist = sourceToTarget . entries <$> seedMaps

solution1 :: String -> Int
solution1 file = minimum $ getAllLocations file

solution2 :: String -> Int
solution2 file =
  minimum $ uncurry (minimumForRange finalSeedMap) <$> rangePairs initSeeds
  where
    (initSeeds, seedMaps) = parseFile file
    finalSeedMap = mconcat seedMaps
    rangePairs :: [Int] -> [(Int, Int)]
    rangePairs (x0:rng:xs) = (x0, x0 + rng) : rangePairs xs
    rangePairs [] = []

getSolutions5 :: String -> IO (Int, Int)
getSolutions5 filename = do
  file <- readFile filename
  return (solution1 file, solution2 file)
