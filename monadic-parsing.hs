{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.List
import Control.Monad
import Data.Maybe

newtype Parser a = MkParser { runParser :: String -> Maybe (String, a) }
    deriving (Functor)
-- Also einfach: type Parser = StateT String Maybe

instance Monad Parser where
    return x = MkParser $ \s -> Just (s, x)
    m >>= f  = MkParser $ \s ->
        case runParser m s of
            Nothing     -> Nothing
            Just (s',x) -> runParser (f x) s'



----------- Beispielanwendung: S-Ausdrücke parsen

data Exp
    = Atom String
    | List [Exp]
    deriving (Show,Eq)

parseExp :: Parser Exp
parseExp = choice [ parseSymbol, parseList ]

parseSymbol :: Parser Exp
parseSymbol = fmap Atom $ many1 alphaNum' `andThen` spaces
    where
    alphaNum' = choice [ alphaNum, char '+', char '*' ]

parseList :: Parser Exp
parseList = do
    token "("
    elems <- many parseExp
    token ")"
    return $ List elems

-- Wenn wir schon dabei sind, können wir Ausdrücke auch noch auswerten.
eval :: Exp -> Integer
eval (Atom x) = read x
eval (List ((Atom "+"):xs)) = sum     $ map eval xs
eval (List ((Atom "*"):xs)) = product $ map eval xs

run :: String -> Integer
run = eval . snd . fromJust . runParser (parseExp `andThen` eof)

example = run "(+ 1 (* 2 3))"



----------- Parserkombinatoren

eof :: Parser ()
eof = MkParser $ \s -> if null s then Just (s, ()) else Nothing

char :: Char -> Parser Char
char x = MkParser $ \s ->
    case s of
        (x':s') | x == x' -> Just (s', x)
        otherwise         -> Nothing

string :: String -> Parser String
string x = MkParser $ \s ->
    if x `isPrefixOf` s
        then Just (drop (length x) s, x)
        else Nothing

choice :: [Parser a] -> Parser a
choice [] = MkParser $ const Nothing
choice (m:ms) = MkParser $ \s ->
    case runParser m s of
        Nothing     -> runParser (choice ms) s
        Just (s',x) -> Just (s',x)

many :: Parser a -> Parser [a]
many m = choice [ m >>= \x -> liftM (x:) (many m), return [] ]

many1 :: Parser a -> Parser [a]
many1 m = liftM2 (:) m (many m)

oneOf :: [Char] -> Parser Char
oneOf = choice . map char

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = MkParser $ \s ->
    case s of
        (c:cs)    -> if f c then Just (cs,c) else Nothing
        otherwise -> Nothing

digit :: Parser Char
digit = satisfy $ \c -> c >= '0' && c <= '9'

alphaNum :: Parser Char
alphaNum = satisfy $ \c -> (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

skipMany :: Parser a -> Parser ()
skipMany m = many m >> return ()

spaces = skipMany $ satisfy (`elem` " \t\n")

andThen :: Parser a -> Parser b -> Parser a
andThen m n = do
    x <- m
    n
    return x

token :: String -> Parser String
token x = string x `andThen` spaces

{-
  Probleme an solch naivem Parsing:

  * Da wir vielleicht backtracken müssen, behalten wir in choice
    die vollständige Eingabe. Um das zu beheben, sollte man grundsätzlich
    zwischen Parses, die schon Zeichen konsumiert haben, und solche, die das
    nicht haben, unterscheiden. Das ist die zentrale Idee hinter Parsec.

  * Wir können keine guten Fehlermeldungen ausgeben.

  Zu einer praktikablen Bibliothek fehlen natürlich auch noch Kombinatoren zum
  Parsen von Termen mit unterschiedlichen Operatorpräzedenzen.

  Siehe auch:
  https://www.cs.nott.ac.uk/~gmh/monparsing.pdf
  http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.24.5200
  http://cs.anu.edu.au/people/Clem.Baker-Finch/parsec.pdf
  http://www.staff.science.uu.nl/~swier101/Papers/1996/DetErrCorrComPars.pdf
-}
