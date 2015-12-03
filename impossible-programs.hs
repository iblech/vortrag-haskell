{-# LANGUAGE FlexibleInstances #-}
module Main where

import Data.Maybe

epsilon :: ([Bool] -> Bool) -> [Bool]
epsilon p = if p (False : xs)
    then False : xs
    else True  : epsilon (p . (True:))
    where xs = epsilon (p . (False:))

exists :: ([Bool] -> Bool) -> Maybe [Bool]
exists p = if p xs then Just xs else Nothing
    where xs = epsilon p

forall :: ([Bool] -> Bool) -> Bool
forall p = isNothing $ exists (not . p)

instance Eq ([Bool] -> Bool) where
    f == g = forall $ \xs -> f xs == g xs
