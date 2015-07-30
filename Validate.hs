module Validate where

import AbstractSyntax
import Interpret

-- Functions for generating key-value stores for testing
-- purposes given a collection of integers and keys.
dat :: [[String]] -> [Integer] -> KeyValueStore
dat (ks:kss) ns = [(k:ks', n) | (ks', n) <- dat kss ns, k <- ks]
dat []       ns = [([], Number n) | n <- ns]

dats :: [[String]] -> [Integer] -> [KeyValueStore]
dats kss ns = [dat (rotate i kss) (rotate j ns) | i <- [0..length kss-1], j <- [0..length ns-1]]
  where rotate i xs = take (length xs) (drop i (cycle xs))


class Exhaustive a where
  exhaustive :: Integer -> [a]


instance Exhaustive Exp where
	exhaustive a = if a == 1 then [DATA, (Variable "x"), (Variable "y")] else
		[Max x | x <- exhaustive(a-1)]
		++[Min x | x <- exhaustive(a-1)]
		++[Sum x | x <- exhaustive(a-1)]
		++[Product x | x <- exhaustive(a-1)]
		++[Union x | x <- exhaustive(a-1)]
		++[Intersection x | x <- exhaustive(a-1)]
		++[MakeSet x | x <- exhaustive(a-1)]


  -- Complete for Problem 4, part (a).

instance Exhaustive Stmt where
	exhaustive a = if a == 1 then [(Return "x"), (Return "y")] else
		[Assign "x" e s | e <- ((exhaustive (a-1)):: [Exp]), s <- ((exhaustive (a-1)):: [Stmt])]
		++[Assign "y" e s | e <- ((exhaustive (a-1)):: [Exp]), s <- ((exhaustive (a-1)):: [Stmt])]


validate :: Integer -> (Stmt -> Algorithm) -> (Stmt -> Algorithm) -> [KeyValueStore] -> [(Stmt, KeyValueStore)]
validate i f g kvs = [(x, y) | x <- (exhaustive i), y <- kvs, (f x y) /= (g x y)]

--eof