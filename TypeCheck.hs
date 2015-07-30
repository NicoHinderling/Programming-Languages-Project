module TypeCheck where

import AbstractSyntax
import Parse

justValue :: Maybe a -> a
justValue (Just a) = a

isJust         :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

elemei :: String -> [(String, Type)] -> Bool
elemei x (v:vs) = if x == fst v then True else elemei x vs
elemei x [] = False


class Typeable a where
  typeCheck :: [(String, Type)] -> a -> Maybe Type

instance Typeable Exp where 
  typeCheck a (Variable b) = 
    let l = filter (\v -> (fst v) == b) a
    in if length l == 0 then Nothing else
      Just(snd(l!!0))

  typeCheck a (Max b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TyNumber then
      Just TyNumber
    else Nothing

  typeCheck a (Min b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TyNumber then
      Just TyNumber
    else Nothing

  typeCheck a (Sum b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TyNumber then
      Just TyNumber
    else Nothing

  typeCheck a (Product b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TyNumber then
      Just TyNumber
    else Nothing

  typeCheck a (Union b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TySet then
      Just TySet
    else Nothing

  typeCheck a (Intersection b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TySet then
      Just TySet
    else Nothing

  typeCheck a (MakeSet b) =
    let v = typeCheck a b
    in if isJust(v) && justValue(v) == TyNumber then
      Just TySet
    else Nothing

  typeCheck a (DATA) = Just TyNumber

instance Typeable Stmt where 
  typeCheck env (Return x) = 
      if x `elemei` env then Just TyVoid else Nothing
  typeCheck env (Assign x e s) =
      let v1 = typeCheck env e
      in if isJust v1 then
              let env' = env ++ [(x, justValue (v1))]
              in typeCheck env' s
          else Nothing


liftMaybe :: (a -> b) -> (Maybe a -> Maybe b)
liftMaybe f (Nothing) = Nothing
liftMaybe f (Just x) = Just (f x)
-- Complete for Problem 2, part (c).

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe (Just Nothing) = Nothing
joinMaybe (Just (Just x)) = Just x
joinMaybe Nothing = Nothing
-- Complete for Problem 2, part (c).

tokenizeParseTypeCheck :: String -> Maybe Type
tokenizeParseTypeCheck s = joinMaybe (liftMaybe ((typeCheck ::[(String, Type)] -> Stmt -> Maybe Type) []) (tokenizeParse s))








-- eof