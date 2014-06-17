module Main where

import Control.Applicative
import Control.Monad.State

data OrgNode = Heading String [Block] deriving Show
data Block = Text [String] | Inner OrgNode deriving Show

type Parser a = StateT String Maybe a

-- (not used)
-- deadend :: Parser a
-- deadend = StateT $ \s -> Nothing
-- btw: deadend = mzero

-- (not used)
-- anyChar :: Parser Char
-- anyChar = StateT $ \ s -> isempty s $ Just (head s, tail s)
-- btw: anyChar = parsePred (const True)

isempty :: String -> Maybe a -> Maybe a
isempty s foo = if s == [] then Nothing else foo

-- (not used)
-- islesstwo :: String -> Maybe a -> Maybe a
-- islesstwo s foo = if (tail s == []) then Nothing else foo

parsePred :: (Char -> Bool) -> Parser Char
parsePred foo = StateT $ \s -> isempty s $ if (foo $ head s) 
                                           then Just (head s, tail s)
                                           else Nothing

-- new
char :: Char -> Parser Char
char c = parsePred (== c)

-- (not used)
-- parseMany :: (Char -> Bool) -> Parser String
-- parseMany = many . parsePred 

-- exactly :: Int -> String -> Parser String
-- пусть exactly принимает любой парсер:
exactly :: Int -> Parser a -> Parser [a]
-- exactly 0 c = return []
-- exactly n c = parsePred (`elem` c) >> exactly (n - 1) c
exactly 0 c = return []
exactly n c = do
  x <- c
  y <- exactly (n-1) c
  return $ x:y
-- можно так: (:) <$> c <*> exactly (n-1) c

-- (not used)
-- true :: (a -> Bool)
-- true = \_ -> True

isend :: Parser Char
-- isend = StateT $ \s -> isempty s $ if ("\n" /= take 1 s )
--                                             then Just (head s, tail s)
--                                             else Nothing  
isend = parsePred (/= '\n')

-- new
-- (увеличивает гибкость)
eof :: Parser ()
eof = StateT $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing


-- clearend :: Parser Char
clearend :: Parser ()
-- clearend = StateT $ \s -> if (s == [])
--                           then Just ('e', "")
--                           else
--                               if ("\n" /= take 1 s)
--                               then Nothing
--                               else Just (head s, drop 1 s)
clearend = (char '\n' >> return ()) <|> eof

toend :: Parser String
toend = do
  x <- many isend
  clearend
  return x
-- можно так: toend = many isend <* clearend

spaces :: Parser String
-- spaces = parseMany (\x -> x `elem` " ")
spaces = many (char ' ')

parseStars :: Int -> Parser String
-- parseStars n = exactly n "*" >> spaces >> toend
parseStars n = exactly n (char '*') >> spaces >> toend

parseSpaces :: Int -> Parser String
-- parseSpaces n = exactly n " " >> spaces >> toend
parseSpaces n = exactly n (char ' ') >> spaces >> toend

parseS :: Int -> Parser OrgNode
parseS n = do
  x <- parseStars n
  y <- many $ parseB n
  return $ Heading x y
-- можно так: parseS = Heading <$> parseStars n <*> many (parseB n)

parseB :: Int -> Parser Block
-- parseB n = (many (parseSpaces n) >>= \x -> if x == [] then deadend else return $ Text x)
--            <|>
--            (parseS (n + 1) >>= \x -> return $ Inner x)
parseB n = (some (parseSpaces n) >>= \x -> return (Text x))
           <|>
           (parseS (n + 1) >>= \x -> return $ Inner x)
-- можно так:
-- parseB n = (Text <$> some (parseSpaces n)) <|> (Inner <$> parseS (n+1))

runParseOrg :: String -> Maybe OrgNode
runParseOrg str = case runStateT (parseS 1) str of
--  Nothing -> Nothing
--  Just (k, v) -> Just k
  Just (k, []) -> Just k -- остаток строки должен быть пустым
  _ -> Nothing

main :: IO ()
main = do
  x <- readFile "orgHaskell"
  print (runParseOrg $ x)
