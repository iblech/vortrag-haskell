{-# LANUGAGE ExistentialQuantification, KindSignatures, GADTs, Rank2Types #-}
module Main where

import Control.Monad.Error hiding (lift)
import Data.IORef
import System.Exit

--------------------------------------------------------------------------------
-- Freie Monaden über freien Funktoren über Termkonstruktoren

data Prog t a =
    Pure a | forall r. Step (t r) (r -> Prog t a)

lift :: t a -> Prog t a
lift x = Step x Pure

instance Functor (Prog t) where
    fmap f (Pure x)   = Pure (f x)
    fmap f (Step u k) = Step u (fmap f . k)

instance Monad (Prog t) where
    return = Pure

    Pure x   >>= f = f x
    Step u k >>= f = Step u ((>>= f) . k)


--------------------------------------------------------------------------------
-- Beispiel: Die State-Monade spezifiert über die Signatur ihrer möglichen
-- Nebenwirkungen und eine operationelle Semantik

data StateI :: * -> * -> * where
    Get :: StateI s s
    Put :: s -> StateI s ()

type State s = Prog (StateI s)

get :: State s s
get = lift Get

put :: s -> State s ()
put st = lift (Put st)

runState :: State s a -> s -> (a,s)
runState (Pure x)           st = (x,st)
runState (Step Get       k) st = runState (k st) st
runState (Step (Put st') k) st = runState (k ()) st'

evalState :: State s a -> s -> a
evalState = ((.) . (.)) fst runState


--------------------------------------------------------------------------------
-- Koprodukt von Monaden

data Sum    m n a = Inl (m a) | Inr (n a)
type Coprod m n   = Prog (Sum m n)

inl :: m a -> Coprod m n a
inl x = Step (Inl x) Pure

inr :: n a -> Coprod m n a
inr x = Step (Inr x) Pure

elim
    :: (Monad m, Monad n, Monad s)
    => (forall a. m a -> s a)
    -> (forall a. n a -> s a)
    -> (forall a. Coprod m n a -> s a)
elim phi psi (Pure x)         = return x
elim phi psi (Step (Inl m) k) = phi m >>= elim phi psi . k
elim phi psi (Step (Inr n) k) = psi n >>= elim phi psi . k


--------------------------------------------------------------------------------
-- Beispiel: Koprodukt aus State- und Error-Monade

type Err e = Either e
type M     = Coprod (State Int) (Err String)

ex :: M Int
ex = do
    st <- inl get
    if st <= 0 then inr (Left "Fehler") else do
    inl $ put (st - 1)
    return $ st^2 + st + 1

runM :: Int -> M a -> IO a
runM st m = newIORef st >>= \ref -> elim (embedState ref) embedErr m

embedState :: IORef s -> State s a -> IO a
embedState ref m = do
    st <- readIORef ref
    let (x,st') = runState m st
    writeIORef ref st'
    return x

embedErr :: (Show e) => Err e a -> IO a
embedErr (Left  e) = putStrLn ("Fehler: " ++ show e) >> exitFailure
embedErr (Right x) = return x
