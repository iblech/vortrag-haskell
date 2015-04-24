module Main where

import Control.Monad

-- data Maybe a = Nothing | Just a

newtype Parser a = MkParser { runParser :: String -> Maybe (String,a) }

instance Monad Parser where
    return = pure
    (>>=)  = bind

data Exp = Atom String | List [Exp]
    deriving (Show,Eq)

-- hallo
-- (foo bar baz (...))

parseExp :: Parser Exp
parseExp = choice [ parseAtom, parseList ]

parseAtom :: Parser Exp
parseAtom = do
    x <- many1 alphaNum
    spaces
    return (Atom x)
-- parseAtom = liftM Atom $ many1 alphaNum `andThen` spaces

parseList :: Parser Exp
parseList = do
    token "("
    xs <- many parseExp
    token ")"
    return $ List xs

token :: String -> Parser String
token s = string s `andThen` spaces

string :: String -> Parser String
string s = sequence $ map char s

makarius :: Parser a -> Parser b -> Parser (a,b)
makarius m n = do
    x <- m
    y <- n
    return (x,y)
-- makarius m n = liftM2 (,) m n
-- makarius = liftM2 (,)


alphaNum :: Parser Char
alphaNum = satisfy $ \c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

eof :: Parser ()
eof = MkParser $ \s ->
    case s of
	[]        -> Just ([], ())
	otherwise -> Nothing

char :: Char -> Parser Char
char c = MkParser $ \s ->
    case s of
	(d:cs) | c == d -> Just (cs, c)
	otherwise       -> Nothing

bind :: Parser a -> (a -> Parser b) -> Parser b
bind m f = MkParser $ \s ->
    case runParser m s of
	Just (s', x) -> runParser (f x) s'
	Nothing      -> Nothing

choice :: [Parser a] -> Parser a
choice []     = MkParser $ \s -> Nothing
choice (m:ms) = MkParser $ \s ->
    case runParser m s of
	Just (s', x) -> Just (s', x)
	Nothing      -> runParser (choice ms) s
	

pure :: a -> Parser a
pure x = MkParser $ \s -> Just (s, x)

many :: Parser a -> Parser [a]
many m = choice [ bind m (\x -> bind (many m) (\xs -> pure (x:xs))), pure [] ]

many1 :: Parser a -> Parser [a]
-- many1 m = bind m (\x -> bind (many m) (\xs -> pure (x:xs)))
-- many1 m = m >>= \x -> many m >>= \xs -> return (x:xs)
many1 m = do
    x  <- m
    xs <- many m
    return (x:xs)
-- many1 m = liftM2 (:) m (many m)
-- pointless :-)

{-
irc.freenode.net
/q lambdabot
@pl \x -> x + 3
(+ 3)
-}

andThen :: Parser a -> Parser b -> Parser a
andThen m n = bind m (\x -> bind n (\y -> pure x))

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = MkParser $ \s ->
    case s of
	(c:cs) | f c -> Just (cs, c)
	otherwise    -> Nothing

-- char c = satisfy (== c)

space :: Parser Char
space = satisfy (== ' ')

spaces :: Parser [Char]
spaces = many space

example  = bind (char 'a') (\_ -> char 'b')
example' = bind (char 'a') (\c -> char c)

{-
f $ x = f x
($) = id

map ($ x)
-}
