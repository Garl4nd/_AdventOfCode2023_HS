--import qualified Prelude as P (log)

import qualified Control.Monad.Trans.Reader as R
import Control.Monad.Trans.Class (lift)
import Control.Monad.ST
import Data.Array.ST (STArray, MArray (newArray), runSTArray, writeArray, modifyArray, readArray)
import qualified Data.Array as A
import GHC.Arr (freezeSTArray, unsafeFreezeSTArray)
import Control.Monad (forM_)
import Control.Monad.Trans.Reader (ReaderT(runReaderT))
import Control.Monad.Trans.State (StateT, execState)
import Data.Array.Storable (StorableArray)
newtype State s a = State {runState :: s -> (s, a)}
instance Functor (State s) where
  --fmap :: (a -> b) -> State s a -> State s b 
    fmap f (State runStateO) = State {runState = \s -> let (sn, a) = runStateO s in (sn, f a)}
 
instance Applicative (State s) where
  pure f = State {runState = \s -> (s, f)}-- State s (a->b) -> State s (a) -> State s (b)
  (State runStateO) <*> (State runState1) = State {runState = \s -> let 
                                                    (s1, f) = runStateO s
                                                    (s2, a) = runState1 s1 in 
                                                    (s2, f a)} 
instance Monad (State s) where 
  --State s a -> (a -> State s b) -> State s b
  (State runState0) >>= f = State {runState = \s -> let 
                    (sn, a) = runState0 s
                    in runState (f a) sn}


get :: State s s
get = State (\s -> (s,s))

put :: s -> State s ()
put state = State (const (state, ()))

modify :: (s->s) -> State s ()
modify f = State (\s ->  (f s, ()))

newtype Reader cfg a = Reader {runReader :: cfg -> a}
instance Functor (Reader cfg) where
  fmap f (Reader runReader0)= Reader  {runReader = f.runReader0} 
instance Applicative (Reader cfg) where  
  pure f = Reader  {runReader = const f}
  (Reader runReaderF) <*> (Reader runReaderX) = Reader (\cfg -> runReaderF cfg $ runReaderX cfg)
instance Monad (Reader cfg) where
  -- >>= :: Reader cfg a -> (a -> Reader cfg b) -> Reader cfg b
  (Reader runReader0) >>= f = Reader (\cfg ->  let a = runReader0 cfg
                                                   newReader = f a 
                                                   in   (runReader newReader)    cfg )

ask :: Reader cfg cfg 
ask = Reader id

asks :: (cfg -> a)  -> Reader cfg a
asks f = do 
  cfg <- ask 
  return (f cfg)

stateCalc ::  Reader Int Int
stateCalc = do
  modState <- asks (+5)
  return $ modState * 4

--(runReader stateCalc) 4

local :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
local trans (Reader runReader') = Reader (runReader'.trans) 
  --Eq 1 . Reader {runReader = (\cfg -> runReader transReader $ trans cfg )} 
--Eq. 2 do
 -- cfg <- ask
  --return  (runReader transReader (trans cfg))
  --where (Reader runReader') = transReader
--------------------------------------

newtype Writer log a = Writer {runWriter :: (log, a)}
instance Functor (Writer log) where
  fmap f (Writer (log0, x)) = Writer (log0, f x)

instance (Monoid log) => Applicative (Writer log) where
  pure x = Writer (mempty, x)
  (Writer (l1, f)) <*> (Writer (l2, x)) = Writer (l1 <> l2, f x)

instance (Monoid log) => Monad (Writer log) where
  (Writer (l0, x)) >>= f = Writer (l0 <> l1, y) where
    Writer (l1, y)  = f x

tell :: log -> Writer log ()
tell l = Writer (l, ())

log' :: String -> Writer [String] () 
log' str = tell [str]

logs = tell 

censor :: (log -> log) -> Writer log a -> Writer log a
censor lTrans (Writer (l, x)) = Writer (lTrans l, x)

listen :: Writer log a -> Writer log (a, log)
listen (Writer (l, x)) = Writer (l, (x,l))


addTwo :: Int -> Writer [String] Int
addTwo x = do 
  log' "Adding 2"
  return $ x +2

augmentAndStringify :: Int -> Int -> Writer [String] String
augmentAndStringify x y = do 
  log' "Augmenting..."
  x' <- addTwo x
  (y', yLog) <-  listen $ addTwo y
  log' "Stringifying..."  
  censor (<> ["Log Approved! The log produced by y is"<> (head yLog)])  (return  (show x' <> "+" <> show y'))
  --return $ show x' <> "+" <> show y'



--------------------------------------
reverseWithCount :: [a] -> State Int [a]
reverseWithCount ls = do 
  modify (+1)
  return $ reverse ls

appendReversedWithCount :: [a] -> [a] -> State Int [a]
appendReversedWithCount ls1 ls2 = do 
  rev1 <- reverseWithCount ls1
  rev2 <- reverseWithCount ls2
  modify (+1)
  s <- get
  put (25+s)
  return $ rev1 ++ rev2

--stateCalc = appendReversedWithCount [4,5,6] [7,8,9]
-- runState stateCalc 0
--
type ArReader s = R.ReaderT (STArray s Int Int) (ST s )

arTimes2 :: Int -> A.Array Int Int
arTimes2 x = runSTArray $  do
  ar <- newArray (1,10) 2
  forM_ [1..10]  (\i -> writeArray ar i (x+i))
  return ar

modifyArr ::  Int -> Int -> ArReader s ()
modifyArr idx num = do
  ar <- R.ask
  lift $ modifyArray ar idx (*num)  

makeAr :: A.Array Int Int
makeAr = runSTArray $ do
  ar <- newArray (1,10) 2
  R.runReaderT (modifyArr 3 7) ar 
  return ar
type ArReader2 s = R.ReaderT (STArray s Int Int, STArray s Int Int) (ST s )

arPair :: (A.Array Int Int, A.Array Int Int)
arPair  = runST $  do
  ar1 <- newArray (1,10) 0
  ar2 <- newArray (1,10) 0
  forM_ [1..10]  (\i -> writeArray ar1 i (i))
  forM_ [1..10]  (\i -> writeArray ar2 i (2*i))
  R.runReaderT   (modifyArr2 3 5) (ar1, ar2)
  modifyArray ar1 0 (*5)
  (,) <$> unsafeFreezeSTArray ar1 <*> unsafeFreezeSTArray ar2

modifyArr2 ::  Int -> Int -> ArReader2 s ()
modifyArr2 idx num = do
  (ar1, ar2) <- R.ask
  lift $ modifyArray ar1 idx (*num)
  lift $ modifyArray ar2 idx ((*num).(^2))

type GameState s k = State (ArReader s k, Int ) ()
modifyGS ::  Int -> Int -> GameState s ()
modifyGS idx num = do
  (_, num') <- get
  put (modifyArr idx num, num'+1)

type GameState2 s = State (ST s (STArray s Int Int), Int ) ()
updateState :: GameState2 s 
updateState = do
  (star, x) <- get
  let modAr = do {ar <- star; modifyArray ar 2 (+1);return ar}
  put (modAr, x+3)
--fnc :: (A.Array Int Int, Int)
--fnc = execState (modifyGS 0 1) (pure $ ())
--
run n = let ((stAr, num), _) =  runState (updateStateN) (newArray (0,10000) 0, 0)
            updateStateN = forM_ [1..n] (const updateState)

        in   (runSTArray stAr, num)
