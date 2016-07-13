{-# LANGUAGE ScopedTypeVariables #-}
module MyParser where
import GHC.Unicode (isSpace, isDigit)
import Data.Char (ord)

data Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)]    -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

instance Functor Parser where
  fmap f (Parser a) = Parser (\s -> [(f c, cs) | (c, cs) <- a s])

instance Applicative Parser where
  pure a  = Parser (\s -> [(a, s)])
  -- (Parser f) <*> (Parser a) = Parser (\s -> [(f c, cs) | (c, cs) <- f s])
  -- Apply cs1 to s, get out a list of tuples (a->b, s1), apply cs2 to s1 and get tuples of (a, s2)
  -- create tuples of (f a, s2)
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f  = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s
    -- apply the Parser p to s
    -- then map over tuples of the list [(a,String)]
    -- apply the function f to the a which returns a Parser
    -- apply this Parser to the not consumed String returned by the Parser p
    -- add the returned tuple to a list
    -- do the same for the next entry in the list of tuples
    -- after the last entry, concatenate all list items in the list to items in a List
    -- ==> from [[('a',"rest")],[('b',"rest2")]] to [('a',"rest"),('b',"rest2")]

item :: Parser Char
item = Parser (\cs -> case cs of
                      ""     -> []
                      (c:cs') -> [(c,cs')])
-- chartuple = do { c <- item; d <- item; return (c,d)}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c
  then return c
  else (Parser (\cs -> []))

class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (+:+) :: m a -> m a -> m a

instance MonadZero Parser where
  zero = Parser (\cs -> [])

instance MonadPlus Parser where
  p +:+ q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p +:+ q) cs of
  [] -> []
  (x:xs) -> [x])

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  a <- p
  as <- many (do {sep; p})
  return (a:as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                  where
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b))
                              +++ return a

space :: Parser String
space = many (satisfy isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (satisfy isDigit); return (ord x - ord '0')}
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

--  apply expr " 1 - 2 * 3 + 4 "
-- [(-1,"")]

data CreditCard = CreditCard String deriving Show

cc4digits :: Parser String
cc4digits = do
  c1 <- satisfy isDigit
  c2 <- satisfy isDigit
  c3 <- satisfy isDigit
  c4 <- satisfy isDigit
  return (c1 : c2 : c3 : c4 : [])

cc :: Parser String
cc = do
  d1 <- cc4digits
  char '-'
  d2 <- cc4digits
  char '-'
  d3 <- cc4digits
  char '-'
  d4 <- cc4digits
  return (d1 ++ d2 ++ d3 ++ d4)

parseCC :: String -> Maybe CreditCard
parseCC s =
  case parse cc s of
    [] -> Nothing
    (c:_) -> Just $ CreditCard $ fst c
