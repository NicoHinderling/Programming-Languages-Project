{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module AbstractSyntax where

import Data.List (union, intersect)

data Exp =
    DATA
  | Variable String
  | Max Exp
  | Min Exp
  | Sum Exp
  | Product Exp
  | Union Exp
  | Intersection Exp
  | MakeSet Exp
  deriving (Eq, Show)

data Stmt =
    Assign String Exp Stmt
  | Return String
  deriving (Eq, Show)

data Type =
    TyNumber
  | TySet
  | TyVoid
  deriving (Eq, Show)

data Value =
    Set [Integer]
  | Number Integer
  | Error
  deriving (Eq, Show)


instance Num Value where
  fromInteger a = Number a
  Number a + Number b = Number(a + b)
  _ + _ = Error
  Number a * Number b = Number(a * b)
  _ * _ = Error

instance Ord Value where
  Number a < Number b = a < b
  _ < Number a        = True
  Number a < _        = False
  _ < _ = False
  Number a <= Number b = a <= b
  _ <= Number a       = True
  Number a <= _       = False
  _ <= _ = False  



(\/) :: Value -> Value -> Value
(\/) (Set a) (Set b) = Set (union a b)
(\/) _ _ = Error

(/\) :: Value -> Value -> Value
(/\) (Set a) (Set b) = Set (intersect a b)
(/\) _ _ = Error


-- Type class Foldable for a fold function on data types.
--
--  * The first argument is a constant that will replace all
--    leaf nodes that contain no variable.
--  * The second argument is a function that will be applied to
--    (i.e., and will replace) any variables.
--  * The third argument is the aggregator for combining
--    results of recursive folds.
--  * The fourth argument is the data value that will be folded.

class Foldable a where
  fold :: b -> (String -> b) -> ([b] -> b) -> a -> b

instance Foldable Exp where
  fold b v f (Variable e)     = v e
  fold b v f DATA             = b
  fold b v f (Max e)          = f [fold b v f e]
  fold b v f (Min e)          = f [fold b v f e]
  fold b v f (Sum e)          = f [fold b v f e]
  fold b v f (Product e)      = f [fold b v f e]
  fold b v f (Union e)        = f [fold b v f e]
  fold b v f (Intersection e) = f [fold b v f e]
  fold b v f (MakeSet e)      = f [fold b v f e]

instance Foldable Stmt where
  fold b v f (Return x) = b
  fold b v f (Assign x e s) = f [fold b v f e, fold b v f s]


vars :: Stmt -> [String]
vars s = (fold [] (\x -> [x]) (concat)) s

operations :: Stmt -> Integer
operations s = (fold 0 (\x -> 1) (sum)) s


-- eof