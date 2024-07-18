module N7
  ( getSolutions7
  ) where

import Data.Char (isDigit)
import Data.List (partition, sortBy, sortOn, nub)
import Data.Function (on)

data Card = NumCard Int| T | J | Q | K | A deriving (Eq, Ord, Show)
data Hand = Hand Card Card Card Card Card deriving (Eq, Show)
data HandType  = HighCard | OnePair | TwoPair | ThreeOfKind   | FullHouse | FourOfKind | FiveOfKind deriving (Eq, Ord, Show)

charToCard :: Char -> Card
charToCard c = case c of
    'T' -> T
    'J' -> J
    'Q' -> Q
    'K' -> K
    'A' -> A
    _ | isDigit c -> NumCard $ read (c : "")

parseFile :: String -> [(Hand, Int)]
parseFile file = parseLine <$> lines file where
    parseLine line = (packHand $ charToCard <$> handStr, read bidStr) where
        [handStr, bidStr] = words line

unpackHand :: Hand -> [Card]
unpackHand (Hand a b c d e) = [a, b, c, d, e]

packHand :: [Card] -> Hand
packHand [a, b, c, d, e] = Hand a b c d e

instance Ord Hand where
  compare h1 h2
    | handType h1 < handType h2 = LT
    | handType h1 > handType h2 = GT
    | otherwise = lexicalComparison False (unpackHand h1) (unpackHand h2)

lexicalComparison :: Bool -> [Card] -> [Card] -> Ordering
lexicalComparison jIsJoker (a:rest1) (b:rest2)
  | a == b = lexicalComparison jIsJoker rest1 rest2
  | (jIsJoker && a == J) = LT
  | (jIsJoker && b == J) = GT
  | otherwise = compare a b
lexicalComparison _ _ _ = EQ

compareWJoker :: Hand -> Hand -> Ordering
compareWJoker h1 h2
  | handTypeWJoker h1 < handTypeWJoker h2 = LT
  | handTypeWJoker h1 > handTypeWJoker h2 = GT
  | otherwise = lexicalComparison True (unpackHand h1) (unpackHand h2)

handType :: Hand -> HandType
handType hand =
  case uniqueCardPartition $ unpackHand hand of
    [_] -> FiveOfKind
    [c, _]
      | length c == 4 || length c == 1 -> FourOfKind
    [c, _]
      | length c == 3 || length c == 2 -> FullHouse
    [c, d, e]
      | length c == 3 || length d == 3 || length e == 3 -> ThreeOfKind
    [c, d, _]
      | length c == 2 || length d == 2 -> TwoPair
    [_, _, _, _] -> OnePair
    _ -> HighCard

handTypeWJoker :: Hand -> HandType
handTypeWJoker hand =
  if (J `elem` unpackedHand)
    then maximum $ handType . packHand <$> (jokerReplacedCards unpackedHand)
    else handType hand
  where
    unpackedHand = unpackHand hand

jokerReplacedCards :: [Card] -> [[Card]]
jokerReplacedCards cards =
  case cards of
    [J, J, J, J, J] -> [[A, A, A, A, A]]
    _ -> allCombinations $ options cards
  where
    allCombinations :: [[a]] -> [[a]]
    allCombinations (optList:rest) =
      [opt : restOpt | opt <- optList, restOpt <- allCombinations rest]
    allCombinations [] = [[]]
    options :: [Card] -> [[Card]]
    options cards = map
      (\card -> case card of
         --J -> filter (/= J) $ presentCards cards
         J -> filter (/= J) $ nub cards
         card -> [card])
      cards

-- presentCards :: Eq a => [a] -> [a]
-- presentCards (x:xs) = x : (presentCards $ filter (/= x) xs)
-- presentCards [] = []

uniqueCardPartition :: Eq a => [a] -> [[a]]
uniqueCardPartition [] = []
uniqueCardPartition (x:xs) = (x : matching) : (uniqueCardPartition rest)
  where
    (matching, rest) = partition (\el -> x == el) xs

solution1 :: String -> Int
solution1 file = sum $ zipWith (\rank (_, bid) -> rank * bid) [1 ..] $ sortOn fst hands_bids where
    hands_bids = parseFile file

solution2 :: String -> Int
solution2 file = sum $ zipWith (\rank (_, bid) -> rank * bid) [1 ..] $ sortedCards where 
    sortedCards = sortBy (on compareWJoker fst) $ parseFile file

getSolutions7 :: String -> IO (Int, Int)
getSolutions7 filename = do
  file <- readFile filename
  return (solution1 file, solution2 file)
