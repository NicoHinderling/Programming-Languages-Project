module Interpret where

import AbstractSyntax
import KeyValueStore
import TypeCheck


makeSet :: Value -> Value
makeSet Error      = Error
makeSet (Number n) = Set [n]
makeSet (Set ns)   = Set ns

type KeyValueStore = [([String], Value)]
type Algorithm = KeyValueStore -> Maybe KeyValueStore

eval :: [(String, KeyValueStore)] -> Exp -> Algorithm
eval env (Min e) = liftMaybe suffix . liftMaybe (combine 1 min) . (eval env e)
eval env (Max e) = liftMaybe suffix . liftMaybe (combine 1 max) . (eval env e)
eval env (Sum e) = liftMaybe suffix . liftMaybe (combine 1 (+)) . (eval env e)
eval env (Product e) = liftMaybe suffix . liftMaybe (combine 1 (*)) . (eval env e)
eval env (Union e) = liftMaybe suffix . liftMaybe (combine 1 (\/)) . (eval env e)
eval env (Intersection e) = liftMaybe suffix . liftMaybe (combine 1 (/\)) . (eval env e)
eval env (DATA) = (\s -> Just(s))
eval env (Variable e) = (\s -> lookup e env)
eval env (MakeSet e) = liftMaybe (\s -> [(x, makeSet y) | (x, y) <- s]) . (eval env e)

-- Complete for Problem 3, part (d).

exec :: [(String, KeyValueStore)] -> Stmt -> Algorithm
exec env (Return x) kvs = 
	let a = eval env (Variable x) kvs
	in if a == Nothing then Nothing else
		a

exec env (Assign x e s) kvs = 
     let a = (eval env e kvs)
     in if a == Nothing then Nothing else
     	let env' = (env++[(x, justValue a)])
        in let p = exec env' s kvs
        in if p == Nothing then Nothing else p



typeCheckInterpret :: Stmt -> Algorithm
typeCheckInterpret s kvs =

    let v = typeCheck [] s
    in if isJust (v) && justValue(v) == TyVoid then
        exec [] s kvs
    else Nothing


--eof




--typeCheckInterpret (Assign "u" (Product (Min DATA)) (Assign "v" (Product (Min (Variable "u"))) (Return "v"))) ((dats [["T"], ["C", "D"], ["X", "Y", "Z"], ["a", "b"]] [-1,2,3,5]) !! 3)

