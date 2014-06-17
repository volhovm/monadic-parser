{-# LANGUAGE NoImplicitPrelude, ParallelListComp #-}
module NewParser where

import Prelude((+), (*), map, concat, (++), String(..))

($) a b = a b 
hole = hole

data Maybe a = Just a | Nothing
data List a = Nil | Nd a (List a) 
data State s a = State {
     runState :: s -> (a, s)
}

class Functor f where
      fmap :: (a -> b) -> f a -> f b     

instance Functor Maybe where
      fmap f Nothing = Nothing
      fmap f (Just a) = Just (f a)

instance Functor List where
      fmap f Nil = Nil
      fmap f (Nd a b) = Nd (f a) (fmap f b) 

instance Functor [] where
      fmap f [] = []
      fmap f (x:xs) = (f x) : (fmap f xs)

--      fmap f st = State $ \a -> let (u, _) = runState st a
--                                in (f u, a)

------------------------------------------------------------
class (Functor f) => Applicative f where
      pure :: a -> f a
      (<*>) :: f (a -> b) -> f a -> f b
      (<$>) = fmap
      (<$>) :: (a -> b) -> f a -> f b

instance Applicative Maybe where
      pure a = Just a
      Nothing <*> _ = Nothing
      Just f <*> s  = fmap f s

instance Applicative [] where
      pure a = [a]
      (<*>) lst a = [f b | f <- lst | b <- a]

-------------------------------------------------------------
--return x >>= f == f x
--m >>= return == m
--(m >>= f) >>= g == m >>= (\x -> f x >>= g)
class Monad m where
      return :: a -> m a
      (>>=) :: m a -> (a -> m b) -> m b
      (>>) :: m a -> m b -> m b
      (>>) a b = a >>= \_ -> b

instance Monad Maybe where
      return a = Just a
      (Just a) >>= f = f a
      Nothing >>= _ = Nothing

instance Monad [] where
      return a = [a]
      [] >>= _ = []
      l >>= f = concat $ map f l

instance Monad (State a) where
      return a = State $ \s -> (a, s) 
      (>>=) st f = State $ \s -> let (dt, newstate) = runState st s
                                 in runState (f dt) newstate
-------------------------------------------------------------
--mzero 'mplus' m == m
--m 'mplus' mzero == m
--mzero >>= m == mzero
--m >>= mzero == mzero
--m 'mplus' (a 'mplus' c) == (m 'mplus' a) 'mplus' c
class (Monad m) => MonadPlus m where
      mzero :: m a
      mplus :: m a -> m a -> m a

instance MonadPlus Maybe where
      mzero = Nothing
      mplus Nothing m = Nothing
      mplus m _ = m

instance MonadPlus [] where
      mzero = []
      mplus = (++)

--instance MonadPlus State where
         
-------------------------------------------------------------

type Error = String
data Either a b = Left a | Right b

--Parses lists of c into items of a
data Monstupar c a = Monstupar {
      runParser :: [c] -> Either ([c], Error) ([c], a)
}

instance Monad (Monstupar c) where
      return a = Monstupar $ \s -> Right (s, a)
      (>>=) ps f = Monstupar $ \s -> case runParser ps s of
            Left z -> Left z
            Right (u, i) -> (runParser (f i)) u

--creates mstp that returns left with error msg
oops :: String -> Monstupar c a
oops str = Monstupar $ \s -> Left (s, str)

try :: Monstupar c a -> Monstupar c (Maybe a)
try prs = do
    x <- prs
    case x of
         Left x -> Nothing x


