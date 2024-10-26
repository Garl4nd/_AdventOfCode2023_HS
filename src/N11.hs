module N11
  ( getSolutions11
  ) where

import Data.List (elemIndices, intercalate, transpose)

--{# --LANGUE}
s =
  "...#......\n\
 \.......#..\n\
 \#.........\n\
 \..........\n\
 \......#...\n\
 \.#........\n\
 \.........#\n\
 \..........\n\
 \.......#..\n\
 \#...#....."

type Coords = (Int, Int)

expandFile :: String -> String
expandFile file = intercalate "\n" expanededFile
  where
    lns = lines file
    expandedRows = expand $ lns
    expanededFile = transpose $ expand $ transpose expandedRows
    expand :: [String] -> [String]
    expand [] = []
    expand (current:rest) =
      if elemIndices '#' current == []
        then current : current : (expand rest)
        else current : expand rest

galaxiesPerRow :: String -> [[Coords]]
galaxiesPerRow file =
  [[(y, x) | x <- elemIndices '#' ln] | (y, ln) <- zip [0 ..] $ lines file]

galaxiesPerColumn :: String -> [[Coords]]
galaxiesPerColumn file =
  [ [(x, y) | x <- elemIndices '#' ln]
  | (y, ln) <- zip [0 ..] $ transpose (lines file)
  ]

augCoord :: Int -> [[Coords]] -> [Int]
augCoord augNum coords =
  scanl
    (\coord row ->
       if null row
         then coord + augNum
         else coord + 1)
    0
    coords

augmentedGalaxyCoords :: String -> Int -> [Coords]
augmentedGalaxyCoords file augNum = remappedCoords
  where
    remappedCoords = [(remapY y, remapX x) | (y, x) <- concat coordRows]
    coordRows = galaxiesPerRow file
    coordColumns = galaxiesPerColumn file
    augYs = augCoord augNum coordRows
    augXs = augCoord augNum coordColumns
    remapX = (augXs !!)
    remapY = (augYs !!)

getMinDists :: [Coords] -> [Int]
getMinDists coords = minDist <$> pairs
  where
    pairs =
      [ (coords !! i, coords !! j)
      | i <- [0 .. length coords - 1]
      , j <- [i + 1 .. length coords - 1]
      ]
    minDist ((x1, y1), (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

solution1' :: String -> Int
solution1' file =
  sum $ getMinDists $ (concat $ galaxiesPerRow $ expandFile file) --sum $ getMinDists $ galaxyCoords file

solution1 :: String -> Int
solution1 file = sum $ getMinDists $ augmentedGalaxyCoords file 2 --sum $ getMinDists $ galaxyCoords file

solution2 :: String -> Int
solution2 file = sum $ getMinDists $ augmentedGalaxyCoords file 1000000 --sum $ getMinDists $ galaxyCoords file

getSolutions11 :: String -> IO (Int, Int)
getSolutions11 filename = do
  file <- readFile filename
    --print $ galaxyCoords file
  writeFile "outputs\\expandedGalaxy.txt" $ expandFile file
  return (solution1 file, solution2 file)
