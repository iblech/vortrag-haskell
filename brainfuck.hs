module Main where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.Char
import Text.ParserCombinators.Parsec

data Stmt
    = IncPtr | DecPtr | IncVal | DecVal
    | PutChar | GetChar | While [Stmt]
    | Nop
    deriving (Show,Eq)

data St = MkSt
    { ptr :: Int
    , mem :: M.Map Int Int
    }
    deriving (Show,Eq)

type EvalM = StateT St IO

initialSt :: St
initialSt = MkSt
    { ptr = 0
    , mem = M.empty
    }

eval :: Stmt -> EvalM ()
eval IncPtr = modify $ \st -> st { ptr = ptr st + 1 }
eval DecPtr = modify $ \st -> st { ptr = ptr st - 1 }
eval IncVal = modify $ \st -> st { mem = M.insert (ptr st) (look 0 (ptr st) (mem st) + 1) (mem st) }
eval DecVal = modify $ \st -> st { mem = M.insert (ptr st) (look 0 (ptr st) (mem st) - 1) (mem st) }
eval PutChar = do
    st <- get
    lift $ putChar $ chr $ look 0 (ptr st) (mem st)
eval GetChar = do
    x <- lift getChar
    modify $ \st -> st { mem = M.insert (ptr st) (ord x) (mem st) }
eval (While ss) = do
    st <- get
    if look 0 (ptr st) (mem st) /= 0
        then mapM_ eval ss >> eval (While ss)
        else return ()
eval Nop = return ()

look :: (Ord a) => b -> a -> M.Map a b -> b
look def x v
    | Just y <- M.lookup x v = y
    | otherwise              = def

parseBrainfuck :: String -> Either ParseError [Stmt]
parseBrainfuck input = parse (many parseStmt) "(unknown)" input

run :: String -> IO ()
run input = do
    case parseBrainfuck input of
        Right ss -> evalStateT (mapM_ eval ss) initialSt
        Left  e  -> putStrLn . show $ e

parseStmt :: GenParser Char st Stmt
parseStmt = choice [parseSimpleStmt, parseWhileStmt, parseDummy]

parseSimpleStmt :: GenParser Char st Stmt
parseSimpleStmt = do
    symbol <- oneOf "><+-.,"
    case symbol of
        '>' -> return IncPtr
        '<' -> return DecPtr
        '+' -> return IncVal
        '-' -> return DecVal
        '.' -> return PutChar
        ',' -> return GetChar

parseWhileStmt :: GenParser Char st Stmt
parseWhileStmt = do
    char '['
    body <- many parseStmt
    char ']'
    return $ While body

parseDummy :: GenParser Char st Stmt
parseDummy = do
    noneOf "><+-.,[]"
    return Nop

helloWorldBF :: String
helloWorldBF =
  "++++++++++\
  \[\
  \  >+++++++>++++++++++>+++>+<<<<-\
  \]                       Schleife zur Vorbereitung der Textausgabe\
  \>++.                    Ausgabe von 'H'\
  \>+.                     Ausgabe von 'e'\
  \+++++++.                'l'\
  \.                       'l'\
  \+++.                    'o'\
  \>++.                    Leerzeichen\
  \<<+++++++++++++++.      'W'\
  \>.                      'o'\
  \+++.                    'r'\
  \------.                 'l'\
  \--------.               'd'\
  \>+.                     '!'\
  \>.                      Zeilenvorschub\
  \+++.                    Wagenrücklauf"
