{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables, TemplateHaskell #-}
module PIOM where

import Prelude
import Control.Exception (try, SomeException)

hole = hole

data State s a = State {
     runState :: s -> (Maybe a, s)
}

instance Monad (State s) where
     return a = State $ \s -> (Just a, s)
     st >>= f = State $ \s -> let (u, i) = runState st s
           in case u of
              Nothing -> (Nothing, i)
              Just a -> (runState (f a)) i
                      
class (Monad m) => MonadZero m where
     mzero :: m a

instance MonadZero (State s) where
     mzero = State $ \s -> (Nothing, s)

type PIO = State String

readToMaybe :: (Read s) => String -> Maybe s
readToMaybe str = case (reads str) of
                       [(c, "")] -> Just c
                       _ -> Nothing

readInteger :: PIO Integer
readInteger = State $ \s -> let (x:xs) = words s
                               in (readToMaybe x :: Maybe Integer, unwords xs)

readFloat :: PIO Float
readFloat = State $ \s -> let (x:xs) = words s
                               in (readToMaybe x :: Maybe Float, unwords  xs)

runPIO :: String -> PIO a -> Maybe a
runPIO str pio = let (u,_) = runState pio str in u



---------------------------------------------
fiveOfThem = do
           x <- readInteger
           y <- readFloat
           z <- readInteger
           a <- readFloat
           b <- readInteger
           return (x, y, z, a, b)

fiveRun str = runPIO str fiveOfThem

runPIO1 :: (Num a) => String -> PIO a -> a 
runPIO1 str pio = let (u, i) = runState pio str
                 in case u of
                         Just j -> j
                         Nothing -> 0 

readFIF ::  String -> Float
readFIF str = runPIO1 str (readFloat >>= \x -> readInteger >>= \y -> readFloat >>= \z -> return z)

