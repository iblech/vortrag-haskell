{-# LANGUAGE RankNTypes, ExistentialQuantification, GADTs #-}
module Main where

import Control.Monad (join)

--------------------------------------------------------------------------------
-- Freie Monaden

data Free f a
    = Pure a
    | Roll (f (Free f a))

liftF :: (Functor f) => f a -> Free f a
liftF = Roll . fmap Pure

instance (Functor f) => Functor (Free f) where
    fmap h (Pure x) = Pure (h x)
    fmap h (Roll u) = Roll (fmap (fmap h) u)

instance (Functor f) => Monad (Free f) where
    return x = Pure x
    m >>= k  = join_ $ fmap k m
        where
        join_ (Pure u) = u
        join_ (Roll v) = Roll (fmap join_ v)

can :: (Functor f, Monad m)
    => (forall a. f a -> m a)
    -> (forall a. Free f a -> m a)
can phi (Pure x) = return x
can phi (Roll u) = join $ phi . fmap (can phi) $ u
          -- oder: join $ fmap (can phi) . phi $ u


--------------------------------------------------------------------------------
-- Anwendung: State als Quotient einer freien Monade

data StateInstr s r
    = Put s r
    | Get (s -> r)

type State s = Free (StateInstr s)
-- ein Wert dieses Typs könnte so aussehen:
--
--     Roll $ Put x $ Roll $ Put x' $ Roll $ Get $ \x'' -> Pure (42+x'')

get :: State s s
get = Roll $ Get Pure

put :: s -> State s ()
put x = Roll $ Put x $ Pure ()

evalState :: State s a -> s -> (a,s)
evalState (Pure x) s = (x,s)
evalState (Roll u) s
    | Get k     <- u = evalState (k s) s
    | Put s' u' <- u = evalState u'    s'


--------------------------------------------------------------------------------
-- Anwendung: Reader als Quotient einer freien Monade

data ReaderInstr e r
    = Ask (e -> r)

type Reader e = Free (ReaderInstr e)

ask :: Reader e e
ask = Roll $ Ask Pure

local :: e -> Reader e a -> Reader e a
local e m = Pure $ evalReader m e

evalReader :: Reader e a -> e -> a
evalReader (Pure x) e = x
evalReader (Roll u) e
    | Ask k <- u = evalReader (k e) e


--------------------------------------------------------------------------------
-- "Noch freier"

data FreeFunctor f a = forall x. MkFreeF (f x) (x -> a)
-- Links-Kan-Erweiterung von f längs Identität.
-- Ist der "freie Funktor über f".

univ :: (Functor g) => (forall a. f a -> g a) -> (FreeFunctor f a -> g a)
univ phi (MkFreeF u k) = fmap k (phi u)


--------------------------------------------------------------------------------
-- Anwendung: State noch freier

data StateInstr' s r where
    Get' :: StateInstr' s s
    Put' :: s -> StateInstr' s ()

instance Functor (FreeFunctor f) where
    fmap phi (MkFreeF u k) = MkFreeF u (phi . k)

type State' s = Free (FreeFunctor (StateInstr' s))

evalState' :: State' s a -> s -> (a,s)
evalState' (Pure x) s = (x,s)
evalState' (Roll (MkFreeF m k)) s =
    case m of
        Get'    -> evalState' (k s) s
        Put' s' -> evalState' (k ()) s'
