{-# LANGUAGE  OverloadedStrings #-}
module N24
    (   getSolutions24
    ) where
import ParsingFuncs  (splitOn)
import Data.List ( intercalate ) 
data Vector = Vector Double Double Double deriving (Show, Eq)
data Particle = Particle {position:: Vector, velocity:: Vector} deriving (Show, Eq)
data Coeffs = Coeffs {a:: Double, b::Double, c:: Double} deriving (Show, Eq)


data Vect2D = Vect2D Double Double
data Rectangle = Rectangle Vect2D Vect2D
data Intersection = Point Double Double | None deriving (Show)

class VectNumeric a where
    infixl 6 |+|
    infixl 6 |-|
    infixl 7 |*|
    infixl 7 |/|

    (|+|) :: a -> a -> a
    (|*|) ::  Double ->a  -> a    
    neg' :: a-> a    
    (|-|) :: a -> a -> a
    x |-| y = x |+| neg' y
    (|/|) ::   a -> Double  -> a
    x |/| y = (1/y) |*| x 
instance VectNumeric Vector where 
--(<+>) :: Vector -> Vector -> Vector
    (Vector a b c) |+| (Vector a' b' c') = Vector (a+a') (b+b') (c+c')
    neg' (Vector a b c) = Vector (-a) (-b) (-c)
    s  |*| (Vector a b c) = Vector (s*a) (s*b) (s*c)
instance VectNumeric Particle where
    Particle {position = position, velocity = velocity} |+|  Particle {position = position', velocity = velocity'} =   Particle {position = position |+| position', velocity = velocity |+| velocity'}
    neg' Particle {position = position, velocity = velocity} =  Particle {position = neg' position, velocity = neg' velocity}
    s  |*| Particle {position = position, velocity = velocity} = Particle {position = s |*| position, velocity = s |*| velocity} 

normalVector :: Vector -> Vector
normalVector (Vector u v q) = Vector (-v) u q

getCoefs :: Particle -> Coeffs
getCoefs (Particle (Vector x y _)  tanVec)  =  Coeffs {a = n_x, b = n_y, c = - (n_x * x + n_y *y) } where
    Vector n_x n_y _ = normalVector tanVec

areParallel :: Coeffs ->Coeffs ->Bool
areParallel Coeffs {a = a, b = b} Coeffs {a = a', b = b'} = a * b' - a' * b == 0

particleIntersection :: Particle  -> Particle -> Intersection 
particleIntersection ptc1 ptc2 =  coefIntersection (getCoefs ptc1) (getCoefs ptc2) where
            coefIntersection :: Coeffs ->Coeffs -> Intersection 
            coefIntersection coeffs coeffs' 
                | areParallel coeffs coeffs' = None
                | Coeffs {a = a, b = b, c = c} <- coeffs, Coeffs {a = a', b = b', c = c'} <- coeffs' =  
                    let det = (a'*b - b'*a) in Point ((b'*c - c'*b)/det) (-(a'*c - c'*a)/det) 

willCrossIn :: Rectangle -> Particle -> Particle -> Bool 
willCrossIn rect part1 part2 =  inRectangle rect intersectionPoint   && arrivalTime part1 intersectionPoint >0 && arrivalTime part2 intersectionPoint >0 
    where       
        intersectionPoint = particleIntersection part1 part2
        inRectangle (Rectangle (Vect2D x0 y0) (Vect2D x1 y1)) (Point x y)   = x0 <= x && x<= x1 && y0 <= y && y <= y1
        inRectangle  _ None = False

uniquePairs :: [a] -> [(a,a)]
uniquePairs xs = [(xs!! k, xs!! l) | (k,l) <- uniquePairIdxs $ length xs] where
    uniquePairIdxs :: Int -> [(Int, Int)]
    uniquePairIdxs n = [(k, l) |  k<- [0..n-1], l <- [k+1..n-1]  ]

parseParticles:: String -> [Particle]
parseParticles file = map getParticle (lines file) where
    getParticle line = Particle (vectFromString posString) (vectFromString velString) where
        vectFromString str = let [x, y, z] =  read <$>  splitOn ',' str in Vector x y z
        [posString, velString]  = splitOn '@' line

boundingRectangle:: Double-> Double-> Rectangle
boundingRectangle l u = Rectangle (Vect2D l l) (Vect2D  u u)

numOfIntersections :: [Particle] -> Int
numOfIntersections particles = countIf ((uncurry.willCrossIn) $ boundingRectangle 200000000000000 400000000000000) particlePairs where
    countIf p x = length $ filter p x
    particlePairs = uniquePairs particles
    --particles = parseParticles file

particlesPairsF :: String ->  [(Particle, Particle)]
particlesPairsF file = particlePairs where    
    particlePairs =  uniquePairs particles
    particles = parseParticles file

arrivalTime :: Particle -> Intersection -> Double
arrivalTime (Particle (Vector x y _) (Vector u v _ )) (Point xTarget yTarget ) = if u /= 0.0 then (xTarget  - x) /u else (yTarget - y) /v
arrivalTime _ _ = 0

transformParticlesBy :: Particle -> [Particle] -> [Particle]
transformParticlesBy ref  = map (|-| ref )

crossProd :: Vector -> Vector -> Vector
(Vector a b c) `crossProd`  (Vector a' b' c') = Vector (b*c' - b'*c) (c*a' - c'* a) (a * b' - b * a')

dotProd :: Vector -> Vector -> Double
(Vector a b c) `dotProd`  (Vector a' b' c') = a*a' + b*b' + c*c'

getTFrom :: [Particle] -> Int ->Int -> Int
getTFrom ps i j = round  $ (  (xi `crossProd` xj) `dotProd` vj) / ( (vi `crossProd` vj) `dotProd` xj)
        where xi = position $ ps !! i
              xj = position $ ps !! j
              vi = velocity $ ps !! i
              vj = velocity $ ps !! j 
                  
getTs :: [Particle] -> [Int]
getTs particles = (getTFrom particles 0 1 ): map (\i -> getTFrom particles i 0) [1..length particles-2]

findInitialStonePosition :: [Particle] -> Vector
findInitialStonePosition particles =  x0  where 
    --particles = parseParticles file
    lastParticle = last particles 
    particles' = transformParticlesBy lastParticle particles
    Particle {position = x1', velocity= v1'} =  particles' !! 0
    Particle {position = x2', velocity= v2'} = particles' !!1  
    x0 = position lastParticle |+| x0' where
        x0' = (t2 |*| (x1' |+| t1 |*| v1') |-| t1 |*| (x2' |+|  t2 |*| v2'))        
                                    |/| (t2 - t1)  
        t1 = fromIntegral (ts  !! 0)
        t2 = fromIntegral (ts  !! 1)    
        ts = getTs particles'       

solution1 :: String -> Int
solution1 = numOfIntersections.parseParticles

solution2 :: String -> Int
solution2 file =  let Vector x y z = x0 in round $ x+y+z  where 
    x0 = findInitialStonePosition $ parseParticles file
    
        
    
getSolutions24 :: String -> IO (Int, Int)
getSolutions24 inputFile = do 
    file <- readFile inputFile 
    let particles  = parseParticles file
    let particlePairs = particlesPairsF file
    writeFile "outputs/particles.txt" $ intercalate "\n" $ show <$> particles
    writeFile "outputs/particlePairs.txt" $  intercalate "\n" $ show <$>  reverse (take 100 $ reverse  particlePairs)
    --writeFile "../outputs/intersections.txt" $  intercalate "\n" $ show <$>  allIntersections file
    return   (solution1 file, solution2 file)

    
-- calcTs :: String -> IO [Int]
-- calcTs inputFile = do 
--     file <- readFile inputFile 
--     let particles  = parseParticles file
--     let particlePairs = particlesPairsF file
--     let solution1 = numOfIntersections file
--     writeFile "../outputs/particles.txt" $ intercalate "\n" $ show <$> particles
--     writeFile "../outputs/particlePairs.txt" $  intercalate "\n" $ show <$>  reverse (take 100 $ reverse  particlePairs)
--     --writeFile "../outputs/intersections.txt" $  intercalate "\n" $ show <$>  allIntersections file
--     let firstParticle = head particles
--     let particles' = transformParticlesBy firstParticle particles

--     return  $ getTs particles' 
 

-- coefsAreConsistent :: Particle -> Bool
-- coefsAreConsistent particle = let Coeffs {a = a, b= b, c =c } = getCoefs particle
--                                   Particle (Vector x y _) _ = particle in  
--                                    a*x + b*y + c == 0

-- tsNumDenum :: [Particle] -> Int ->Int -> (Double, Double)
-- tsNumDenum ps i j = (((xi `crossProd` xj) `dotProd` vj), ((vi `crossProd` vj) `dotProd` xj))
--         where xi = position $ ps !! i
--               xj = position $ ps !! j
--               vi = velocity $ ps !! i
--               vj = velocity $ ps !! j 


-- --test :: String -> IO [(Double, Double)]
-- test :: String -> IO Bool
-- --test :: String -> IO [Int]
-- test inputFile = do 
--     file <- readFile inputFile 
--     let particles  = parseParticles file
--     let  firstParticle = head particles
--     let  particles' = transformParticlesBy firstParticle particles
--     --let t2From4 = getTFrom particles' 2 4
--     --let t2From3 = getTFrom particles' 2 3
--     let ts = map (getTFrom particles' 2) [3..300]    
--     let ndPairs=  map (tsNumDenum particles' 10) [3..300]
--     return $ all (== head ts) ts 
--     --return ndPairs
--     --return ts            
