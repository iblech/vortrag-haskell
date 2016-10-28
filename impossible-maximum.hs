module Main where

type Nat = Integer

phi :: ([Bool] -> Nat) -> [Bool]
phi f = if f xs >= f ys then xs else ys
    where
    xs = False : phi (f . (False:))
    ys = True  : phi (f . (True:))

maxValue :: ([Bool] -> Nat) -> Nat
maxValue f = f (phi f)
