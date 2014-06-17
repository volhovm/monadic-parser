{-# LANGUAGE NoImplicitPrelude #-}
module Labmda where

import Prelude
import Control.Monad
import Control.Monad.State
import Data.Char
import Control.Applicative

type Parser a = StateT String Maybe a

hole = hole

isempty :: String -> Maybe a -> Maybe a
isempty str foo = if (str == []) then Nothing else foo

byPred :: (Char -> Bool) -> Parser Char
byPred foo = StateT $ \s -> isempty s $ if (foo (head s)) then Just (head s, tail s) else Nothing
  
char :: Char -> Parser Char -- =='c'
char c = byPred (== c)

anyChar :: Parser Char -- const True
anyChar = byPred (const True)

anyOf :: String -> Parser Char
anyOf s = byPred (`elem` s)

spaces :: Parser String
spaces = many $ byPred (== ' ')

exactly :: Int -> Parser a -> Parser [a]
exactly 0 p = return []
exactly n p = do
  x <- p
  y <- exactly (n - 1) p
  return (x:y)

eof :: Parser ()
eof = StateT $ \s -> case s of
  [] -> Just ((), [])
  _ -> Nothing

parseInteger :: String -> Int
parseInteger s = let ret = runStateT (many $ anyOf "-xABCDEF1234567890") s
                 in case ret of
                   Nothing -> 0 :: Int
                   Just (i, o) -> read i :: Int

manysepby :: Parser a -> Parser b -> Parser [a]
manysepby p g = do
  x <- p
  y <- many (g >> p)
  return (x:y)

lexem :: Parser a -> Parser a
lexem p = do
  x <- p
  spaces
  return x

--parseIntSum :: String -> Maybe Int
--parseIntSum s = fmap (foldr1 (+)) $ read <$> runStateT (many $ lexem $ anyOf "-xABCDEF1234567890") s

letters :: Parser String
letters = many (lexem (byPred isLetter))

------------------------------------------------------------

type Vars = String
data LaTerm = Term Vars LaTerm
            | App [LaTerm] 
            | Sole LaTerm
            | Foo String
            deriving Show

parseSole :: Parser LaTerm
parseSole = (Sole <$> (parseApp <|> parseTerm))

parseApp :: Parser LaTerm
parseApp = App <$> manysepby parseTerm (lexem (char '@'))

parseTerm :: Parser LaTerm
parseTerm = (exactly 1 (lexem $ char 'λ') >> Term <$> letters <*> (lexem (char '.') >> parseSole)) <|> (Foo <$> letters)


parseLambda :: String -> LaTerm
parseLambda str = case runStateT parseSole str of
  Just (k, []) -> k
  Nothing -> Foo "error"
  Just (k, v) -> Foo $ "unparsed:'" ++ v ++"'"


------------------------------------------------------------

