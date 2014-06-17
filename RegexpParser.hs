module Main where
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.State

type Parser a = StateT String Maybe a

isenough :: Int -> String -> Maybe a -> Maybe a
isenough n str foo = if (length str < n) then Nothing else foo

bypred :: (Char -> Bool) -> Parser String
bypred foo = StateT $ \s -> isenough 1 s $ if (foo $ head s)
                                              then Just ([head s], tail s)
                                                   else Nothing

text :: String -> Parser String 
text str = StateT $ \s -> isenough (length str) s $ if (str == take (length str) s) 
                                                    then Just (str, drop (length str) s)
                                                    else Nothing
oneof :: String -> Parser String
oneof str = bypred (`elem` str)

char :: Char -> Parser String
char c = bypred (== c)

betw :: Parser a -> Parser b -> Parser c -> Parser b
betw x y z = x >> y >>= \ret -> z >> return ret

num :: Parser String
num = concat <$> (some $ oneof "1234567890")

except :: String -> Parser String
except str =  bypred $ \x -> not $ x `elem` str

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
(<++>) x y = (++) <$> x <*> y

--------------------------------------------------------------------------------

type Quantifier = String 
type Argument = String

--Let's try to parse easy grammar:

data Regexp = Regexp [Block] deriving Show
data Block = Group [Argument] Quantifier | Only Argument deriving Show

usual :: Parser String
usual = except "[]\\"

special :: Parser String
special = text "\\" <++> (foldr1 (<|>) (fmap char specialC))

specialC = ['n', 'R', 'r', 'S', 's', 'd']

getQ :: Parser Quantifier
getQ = ((oneof "+*?") <++> ((char '?') <|> return []))
       <|> ((char '{') <++> ((num <++> (char ',') <++> num) <|> num) <++> (char '}')
            <++> ((char '?' <|> (return []))))
       <|> (return [])

getA :: Parser Argument
getA = (usual <++> (char '-') <++> usual) <|> usual <|> special 

getG :: Parser Block
getG = (Group <$> (betw (char '[') (some getA) (char ']')) <*> getQ) <|> (Only <$> getA)

getReg :: Parser Regexp
getReg = Regexp <$> some getG

runParser :: String -> Regexp
runParser str = case runStateT getReg str of
  Just (u, []) -> u
  Just (u, i) -> Regexp [Only ("PARSE ERROR, " ++ i ++ " WASN'T PARSED")]
  Nothing -> Regexp [Only "PARSE ERROR"]
