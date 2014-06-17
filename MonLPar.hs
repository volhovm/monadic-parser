{-# LANGUAGE NoImplicitPrelude #-}

--Monadic Library Parser (dunno just monpar made of stl)
module MonLPar where

import Prelude 
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.Char

data Hole = Hole
hole = hole

type MLP a = StateT String Maybe a

lastChar :: MLP Char
lastChar = StateT $ \s -> Just (head s, tail s)

err :: MLP Char
err = StateT $ \s -> Nothing

byPred :: (Char -> Bool) -> MLP Char
byPred foo = StateT $ \s -> if (s == []) then Nothing else if (foo (head s)) then Just (head s, tail s) else Nothing

contains :: String -> MLP Char
contains = byPred . flip elem 

--readMany :: [Char] -> MLP [Char]
--readMany list = StateT $ \g -> case (head g `elem` list) of
--  False -> Just (g, "")
--  True -> runStateT (readMany list) $ (head [head [g]]) ++ head [g] ++ tail (head [head [g]])

or' :: MLP a -> MLP a -> MLP a
or' (StateT funct) (StateT funct2) = StateT $ \s -> case funct s of 
                                           Nothing -> funct2 s
                                           _ -> funct s

readMany :: MLP c -> MLP [c]  
readMany a = do
  x <- a
  y <- or' (readMany a) (StateT $ \s -> Just ([], s))
  return (x:y)
