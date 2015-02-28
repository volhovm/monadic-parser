{-# LANGUAGE NoImplicitPrelude #-}
module WordParser where

import Prelude
import Data.Char
import Data.Functor.Identity
import Control.Monad
import Control.Monad.State

--data State s a = State { runState :: s -> (Maybe a, s) }
--class SP, runSP, capital, middle, dotted
--type SP a = State String (Maybe a)
type SP a = StateT String Identity (Maybe a)

--instance Monad (State s) where
 -- return a = StateT $ \s -> (Just a, s)
  --st >>= f = StateT $ \s -> let (u, i) = runState st s
  --                              in case u of
      --                               Nothing -> (Nothing, i)
    --                                 Just a -> runState (f a) i

--instance MonadPlus (State s) where
   -- mzero = StateT $ \s -> (Nothing, s)
    --mplus = undefined


someWord :: (String -> Bool) -> SP String
someWord pred = StateT $ \s -> let (x:xs) = words s
                            in let o = if (pred x) then Just x else Nothing
                                   in Identity (o, unwords xs)


capital :: SP String
capital = someWord $ isUpper . head

middle :: SP String
middle = someWord $ (\x -> True)

dotted :: SP String
dotted = someWord $ ((==) '.') . last

runSP :: String -> SP a -> Maybe a
runSP str sp = fst (runState sp str)
