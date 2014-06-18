{-# LANGUAGE NoImplicitPrelude #-}

module OrgTables where

import Prelude
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Char

type Parser a = StateT String Maybe a

hole = hole

isbigger :: Int -> String -> Maybe a -> Maybe a
isbigger n str foo = if length str >= n then foo else Nothing

byPred :: (Char -> Bool) -> Parser Char
byPred foo = StateT $ \s -> isbigger 1 s $ if (foo $ head s) then Just (head s, tail s) else Nothing

char :: Char -> Parser Char
char c = byPred (== c)

exceptOf :: String -> Parser Char
exceptOf str = byPred (\x -> not (x `elem` str))

eof :: Parser ()
eof = StateT $ \s -> if (s == []) then Just ((), []) else Nothing

pass p = p <|> return []

end :: Parser a -> Parser a
end p = do
  x <- p
  (char '\n' >> return ()) <|> eof
  return x

someSepBy :: Parser a -> Parser b -> Parser [a]
someSepBy p g = do
  x <- p
  y <- some (g >> p)
  return $ x : y

betw :: Parser a -> Parser b -> Parser a
betw g p = do
  p
  x <- g
  p
  return x
--------------------------------------------------------------------------------
 
data Block = Block [Row] 
data Row = Row [String]
instance Show Block where
  show (Block a) = unlines $ map show a
instance Show Row where
  show (Row a) = "|" ++ concat (map (++ "|") a)

sep = char '|'

usual :: Parser String
usual = some $ exceptOf "|\n"

parseRow :: Parser Row
parseRow = end $ (Row <$> (someSepBy usual sep)) `betw` sep
parseSeparator = end $ (someSepBy (some $ char '-') sep) `betw` sep
parseBlock = Block <$> (many $ (parseRow) `betw` (pass parseSeparator))
