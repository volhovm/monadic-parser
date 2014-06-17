{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}
module Main where

import Prelude
import Data.Functor
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.State

hole = hole

data OrgNode = Heading String [Block] deriving Show
data Block = Text [String] | Inner OrgNode deriving Show

type Parser a = StateT String Maybe a

deadend :: Parser a
deadend = StateT $ \s -> Nothing

anyChar :: Parser Char
anyChar = StateT $ \ s -> isempty s $ Just (head s, tail s)

-- >0 elem
isempty :: String -> Maybe a -> Maybe a
isempty s foo = if (s == []) then Nothing else foo

-- >1 elem
islesstwo :: String -> Maybe a -> Maybe a
islesstwo s foo = if (tail s == []) then Nothing else foo

parsePred :: (Char -> Bool) -> Parser Char
parsePred foo = StateT $ \s -> isempty s $ if (foo $ head s) 
                                           then Just (head s, tail s)
                                           else Nothing
                                                                          
parseMany :: (Char -> Bool) -> Parser String
parseMany = many . parsePred 

--passes exactly n symbols from String
exactly :: Int -> String -> Parser String
exactly 0 c = return []
exactly n c = parsePred (`elem` c) >> exactly (n - 1) c

true :: (a -> Bool)
true = \_ -> True

isend :: Parser Char
isend = StateT $ \s -> isempty s $ if ("\n" /= take 1 s )
                                            then Just (head s, tail s)
                                            else Nothing  

clearend :: Parser Char
clearend = StateT $ \s -> if (s == [])
                          then Just ('e', "")
                          else
                              if ("\n" /= take 1 s)
                              then Nothing
                              else Just (head s, drop 1 s)

toend :: Parser String
toend = do
  x <- many isend
  clearend
  return x

spaces :: Parser String
spaces = parseMany (\x -> x `elem` " ")

parseStars :: Int -> Parser String
parseStars n = exactly n "*" >> spaces >> toend

parseSpaces :: Int -> Parser String
parseSpaces n = exactly n " " >> spaces >> toend

parseS :: Int -> Parser OrgNode
parseS n = do
  x <- parseStars n
  y <- many $ parseB n
  return $ Heading x y
    

parseB :: Int -> Parser Block
parseB n = (many (parseSpaces n) >>= \x -> if x == [] then deadend else return $ Text x)
           <|>
           (parseS (n + 1) >>= \x -> return $ Inner x)

runParseOrg :: String -> Maybe OrgNode
runParseOrg str = case runStateT (parseS 1) str of
  Nothing -> Nothing
  Just (k, v) -> Just k

main :: IO ()
main = do
  x <- readFile "orgHaskell.org"
  print (runParseOrg $ x)
