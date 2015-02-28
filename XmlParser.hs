{-# LANGUAGE NoImplicitPrelude #-}
module XMLP where

import Prelude
import Control.Monad
import Control.Applicative
import Control.Monad.State
import Data.Char

hole :: hole
hole = hole

type Parser a = StateT String Maybe a

lengthc :: (Eq a) => [a] -> Int -> Maybe b -> Maybe b
lengthc str n foo = if length str < n then Nothing else foo

bypred :: (Char -> Bool) -> Parser Char
bypred foo = StateT $ \s -> lengthc s 1 $ if foo $ head s then Just (head s, tail s) else Nothing

char :: Char -> Parser Char
char c = bypred (== c)

letters :: Parser String
letters = many $ bypred isLetter

spaces :: Parser String
spaces = many $ char ' '

text :: String -> Parser String
text str = StateT $ \s -> lengthc s (length str) $ if take (length str) s == str
                                                   then Just (str, drop (length str) s)
                                                   else Nothing

between :: Parser a -> Parser b -> Parser c -> Parser b
between f p g = f >> p >>= \x -> g >> return x

regtext :: Parser String
regtext = some (bypred $ \x -> x `notElem` "<>&")

conc :: Parser [a] -> Parser [a] -> Parser [a] -> Parser [a]
conc f g h = do
  x <- f
  y <- g
  z <- h
  return (x ++ y ++ z)

--------------------------------------------------------------------------------

type Heading = String

data Xml = Tag Heading [Block]
data Block = Text String | Inner Xml

opentag = between (text "<") regtext (text ">")
closetag s = text $ "</" ++ s ++ ">"
parseXml = do
  o <- opentag
  g <- many parseBlock
  c <- closetag o
  return $ Tag o g

token = (transf <$> between (text "&") letters (text ";")) <|> return []
parseBlock = (Text <$> conc token regtext token) <|> (Inner <$> parseXml)
parse str = case runStateT (many parseXml) str of
  Just (a, []) -> a
  _ -> [Tag "PARSE ERROR" []]

transf "gt" = ">"
transf "ls" = "<"

--------------------------------------------------------------------------------

instance Show Block where
  show = blockout 0
instance Show Xml where
  show = xmlout 0
xmlout n (Tag str blcks) =
       take (2*n) (cycle " ") ++ "<" ++ str ++ ">\n"
         ++ concatMap (blockout (n + 1)) blcks
         ++ take (2*n) (cycle " ") ++ "</" ++ str ++ ">"

blockout n (Text s) = take (2*n) (cycle " ") ++ s ++ "\n"
blockout n (Inner h) = xmlout n h ++ "\n"

--------------------------------------------------------------------------------

main = do
  x <- readFile "html.in"
  print $
    concatMap show (parse $ concat $ lines x)
