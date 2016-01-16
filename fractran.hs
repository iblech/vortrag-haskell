module Main where

import Data.List
import Data.Ratio
import Control.Monad.State
import Control.Monad.Writer
import Test.QuickCheck
import Control.Exception

type PositiveRat = Rational
type Nat         = Integer
type PositiveNat = Integer
type Program     = [PositiveRat]

run :: Program -> PositiveNat -> [PositiveNat]
run p n = n : maybe [] (run p) (step p n)

step :: Program -> PositiveNat -> Maybe PositiveNat
step []     _ = Nothing
step (x:xs) n
    | Just m <- isInteger (fromIntegral n * x) = Just m
    | otherwise                                = step xs n

isInteger :: Rational -> Maybe Integer
isInteger x
    | denominator x == 1 = Just $ numerator x
    | otherwise          = Nothing

exPrimes :: Program
exPrimes = [17/91, 78/85, 19/51, 23/38, 29/33, 77/29, 95/23, 77/19, 1/17, 11/13, 13/11, 15/14, 15/2, 55/1]

primeFactors :: PositiveNat -> [PositiveNat]
primeFactors = pf primes
    where
    pf ps@(p:ps') n
        | p * p > n = if n == 1 then [] else [n]
        | r == 0    = p : pf ps q
        | otherwise = pf ps' n
        where (q,r) = n `divMod` p

primes :: [PositiveNat]
primes = 2 : filter (null . tail . primeFactors) [3,5..]

encode :: [Nat] -> PositiveNat
encode ns = product $ zipWith (^) primes ns

decode :: PositiveNat -> [Nat]
decode m = if m == 1 then [] else go primes 0 m
    where
    go ps@(p:ps') i n
        | r == 0    = go ps (i+1) q
        | n == 1    = [i]
        | otherwise = i : go ps' 0 n
        where (q,r) = n `divMod` p

encode' :: [Integer] -> PositiveRat
encode' ns = product $ zipWith (^^) (map fromIntegral primes) ns

decode' :: PositiveRat -> [Integer]
decode' m = decode (numerator m) <-> decode (denominator m)
    where
    (<->) = zipWithDefault (-) 0

prop1 :: Positive Nat -> Bool
prop1 n  = let n' = fromIntegral n in encode (decode n') == n'

prop2 :: [Positive Nat] -> Bool
prop2 xs = let xs' = map fromIntegral xs in decode (encode xs') == xs'

prop1' :: Positive Integer -> Positive Integer -> Bool
prop1' x y = let q = fromIntegral x % fromIntegral y in encode' (decode' q) == q

prop2' :: [Integer] -> Bool
prop2' xs = and $ zipWithDefault (==) 0 xs $ decode' (encode' xs)

zipWithDefault :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithDefault (<*>) z []     ys     = map (z <*>) ys
zipWithDefault (<*>) z xs     []     = map (<*> z) xs
zipWithDefault (<*>) z (x:xs) (y:ys) = x <*> y : zipWithDefault (<*>) z xs ys

-- if $n > 0 then ($n, $m) <- (0, $m + $n)
pAdd  n m   = assert (n /= m)           [ m % n ]

-- if $n > 0 then ($n, $m, $p) <- (0, $m + $n, $p + $n)
pAdd' n m p = assert (n /= m && n /= p) [ m * p % n ]

-- if $n > 0 then ($n, $m) <- (0, $m * $n)
pMul        = undefined
