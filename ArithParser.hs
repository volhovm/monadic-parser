{-# LANGUAGE NoImplicitPrelude #-}
module ArithParser where

import Prelude
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Char
import Data.Functor
import System.IO

hole = hole

type Apar a = StateT String Maybe a

isempty :: (Eq b) => [] b -> Maybe a -> Maybe a
isempty s foo = if (s == []) then Nothing else foo

parseByPred :: (Char -> Bool) -> Apar Char
parseByPred foo = StateT $ \s -> isempty s $ if (foo $ head s)
                                           then Just (head s, tail s)
                                           else Nothing 
char :: Char -> Apar Char
char c = parseByPred (== c)

spaces :: Apar String
spaces = many $ char ' '

oneOf :: String -> Apar Char
oneOf str = parseByPred (`elem` str)

lexem :: Apar a -> Apar a
lexem p = do
  x <- p
  spaces
  return x

between :: Apar c -> Apar a -> Apar b -> Apar a
between f p g = do
  f
  x <- p
  g
  return x

exactly :: Int -> Apar a -> Apar [a]
exactly 0 _ = return []
exactly n p = (:) <$> p <*> exactly (n - 1) p

eof :: Apar ()
eof = StateT $ \s -> case s of
  [] -> Just ((), [])
  _ -> Nothing

isend :: Apar Char
isend = parseByPred (/= '\n')

clearend :: Apar ()
clearend = (char '\n' >> return ()) <|> eof

toend :: Apar String
toend = do
  x <- many isend
  clearend
  return x

manySepBy :: Apar a -> Apar b -> Apar [a]
manySepBy p g = (:) <$> p <*> many (g >> p)

parseInt :: Apar Int
parseInt = (many $ lexem $ oneOf "0xABCDEF1234567890") >>= \x -> if x == [] then mzero else return (read x :: Int)

------------------------------------------------------------

data Arith = Sum Arith Arith
           | Mul Arith Arith
           | Num Int
           deriving Show

parseNum :: Apar Arith
parseNum = Num <$> parseInt 

parseSum :: Apar Arith
parseSum = fmap (foldr1 Sum) $ manySepBy parseMul (lexem $ char '+')

parseMul :: Apar Arith
parseMul = fmap (foldr1 Mul) $ manySepBy (parseNum <|> (between (lexem (char '(')) parseSum (lexem (char ')')))) (lexem $ char '*')

eval :: Arith -> Int
eval (Sum a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Num a) = a

parseAr :: String -> Int
parseAr str = case runStateT parseSum str of
  Nothing -> 0 :: Int
  Just (u, i) -> eval u
