{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE  TypeSynonymInstances#-}
{-# LANGUAGE  FlexibleInstances#-}

class Additive a b c | a b -> c where 
    add :: a-> b -> c

instance Additive Int Int Int  where
    add x y = x+ y
instance Additive Int [Char] [Char] where
    add x c =  show x <> "+" <> c

