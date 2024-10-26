import Control.Monad (when, unless)
import qualified Control.Monad.Cont as C
newtype Cont r a = Cont {runCont :: (a->r) ->r}
type ContB r a = (a->r) ->r
addCPS :: Int -> Int -> Cont r Int
addCPS x y = Cont $ \k -> k (x+y)

squareCPS :: Int -> Cont r Int
squareCPS x = Cont $ \k -> k (x*x)

add3CPS :: Int -> Cont r Int
add3CPS x = Cont $ \k -> k (x +3)

squareCPSB :: Int -> ContB r Int
squareCPSB x = \k -> k (x*x)

add3CPSB :: Int -> ContB r Int
add3CPSB x = \k -> k (x +3)

addAndSquareCPSB :: Int -> ContB r Int
addAndSquareCPSB x = \k -> squareCPSB x  (\xsq -> add3CPSB xsq k)  

addAndSquareCPS :: Int -> Cont r Int
addAndSquareCPS x = Cont $ \k -> runCont (squareCPS x)  (\xsq -> runCont (add3CPS xsq) k)  

chainCPS :: (Cont r a) -> (a -> Cont r b) -> Cont r b
chainCPS c f = Cont $ \k -> runCont c (\cRes -> runCont (f cRes) k)

chainCPSB :: (ContB r a) -> (a -> ContB r b) -> ContB r b
chainCPSB c f = \k ->  c (\cRes ->  (f cRes) k)

instance Functor (Cont r) where
    -- fmap :: (a->b) -> Cont r a -> Cont r b
    --fmap f c = Cont $ \k -> runCont c $ \x -> k (f x) 
    fmap f c = Cont $ \k -> runCont c (k.f)

instance Applicative (Cont r) where
    pure x = Cont  ($ x)
    --  <*> :: (Cont r (a->b)) -> Cont r a -> Cont r b
    mab <*> ma = Cont $ \k -> runCont mab (\f -> runCont ma (\x -> k (f x) )) 
instance Monad (Cont r) where
    -- >>=:: (Cont r a) -> (a -> Cont r b) -> Cont r b
    ma >>= f = Cont $ \k -> runCont ma (\x -> runCont (f x) k)

callCC :: ((a-> Cont r b) -> Cont r a) -> Cont r a
--f :: (a-> Cont r b) -> Cont r a
-- h :: (a-> r) 
-- k :: (a-> Cont r b)
callCC f = Cont $ \h -> runCont (f (\x -> Cont (\_ -> h x))) h
--callCC f = Cont $ \h -> runCont (f  (\x -> Cont  $ const (h x))) h
--callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h

chainedCalc :: Cont r Int
chainedCalc = do 
    xsq <- squareCPS 4
    xsqInc <- add3CPS xsq
    x3 <- squareCPS xsqInc
    pure x3


ccEx :: Int -> Cont r Int
ccEx x = callCC $ \k -> do 
    xsq <- squareCPS x
    xsqInc <- add3CPS xsq
    x3 <- squareCPS xsqInc
    when (x3 > 500) $ k 0            
    pure (x3*2)

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
    let y = x ^ 2 + 3
    when (y > 20) $ k "over twenty"
    return (show $ y - 4)

ccExUnpack :: Int -> Cont r Int
ccExUnpack x = Cont $ \h -> runCont (do 
    xsq <- squareCPS x
    xsqInc <- add3CPS xsq
    x3 <- squareCPS xsqInc
    when (x3 > 500) $ ( Cont (\_ -> h 0))            
    pure (x3*2)) h

ccExUPSimple :: Int -> Cont r Int
ccExUPSimple x = Cont $ \h -> runCont ( 
--  (Cont $ \_ -> h 0 ) >>= (\_ -> pure x)
--    Cont $ \c -> runCont (Cont $ \_ -> h 0 ) (\a -> runCont ((\_ -> pure x) $ a) c)
-- Cont $ \c -> runCont (Cont $ \_ -> h 0 ) (\a -> runCont (pure x) c)
 -- (Cont $ \_ -> h 0 ) >>= (\y -> pure (x+y))
  --Cont $ \c -> runCont (if (x>500) then (Cont $ \f -> f () ) else  (Cont $ \_ -> h 0) ) (\_ -> runCont ( pure x) c )
 -- Cont $ \c ->  h 0
  --Cont $ \c -> runCont   (Cont $ \f -> f () ) (\_ -> runCont ( pure x) c) 
   Cont $ \c ->  runCont ( pure x) c 
  ) h


exceptCCEx :: Int -> Cont r (Either String Int) 
exceptCCEx x = callCC $ \ok -> do
  y <- pure (24*(x^2)-x)
  errCode <- callCC $ \notOk -> do
    when (y > 100) $ notOk (Left "Error")
    ok $ Right y
  return errCode
  
--callCCR = callCC
instance C.MonadCont (Cont r) where
    callCC = callCC

data Exception = TooLargeError | NegativeArgumentError deriving Show
throwableSqrt :: Float -> (Exception -> Cont r (Either String Float)) -> Cont r (Either String Float)
throwableSqrt x throw
    | x <0 = throw NegativeArgumentError
    | otherwise = pure  (Right $ sqrt x)
    



tryCC :: C.MonadCont m => ((exc -> m a) -> m a) -> (exc -> m a) -> m a
tryCC throwable errHandler = C.callCC $ \ok -> do  
  err <- C.callCC $ \notOk -> do
    res <- throwable notOk
    ok res
  errHandler err



calcThrowableSqrt :: Float -> Either String Float
calcThrowableSqrt x = runCont (tryCC (throwableSqrt x) (pure.(Left).show))  id

data QuadrRes = NoSolution | SingleSolution Float | DoubleSolution Float Float deriving Show

-- prakticky imperativni kod
solveQuadr :: Float -> Float -> Float -> Cont r QuadrRes
solveQuadr a b c = callCC $ \k -> do
    let dsc = b*b - 4*a*c
    when (dsc <0) $ k NoSolution 
    when (dsc == 0) $ k (SingleSolution ((-b)/(2*a)))
    return $  let sdsc = sqrt(dsc) in DoubleSolution ((-b - sdsc) / (2*a)) ((-b + sdsc)/(2*a) )



