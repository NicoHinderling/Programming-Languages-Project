module ProjectTests where

import AbstractSyntax
import Parse
import TypeCheck
import KeyValueStore
import Interpret
import Validate
import Compile

allTests = [
  show (failed parseTests),
  show (failed typeCheckTests),
  show (failed interpretTests),
  show (failed compileTests)
  ]

-- To get the failures for an individual test, query that
-- test using "failed", e.g.:
-- 
-- *> failed parseTests

failed :: Eq a => [(a, a)] -> [(a, a)]
failed tests = [(x, y) | (x, y) <- tests, x /= y]

parseTests :: [(Maybe Stmt, Maybe Stmt)]
parseTests = [
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = set(DATA); return b;"), Just (Assign "b" (MakeSet DATA) (Return "b"))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign q = set(product(sum(DATA))); assign q = sum(DATA); assign p = q; return q;"), Just (Assign "q" (MakeSet (Product (Sum DATA))) (Assign "q" (Sum DATA) (Assign "p" (Variable "q") (Return "q"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign q = set(product(sum(DATA))); assign q = sum(DATA); assign p = q; return p;"), Just (Assign "q" (MakeSet (Product (Sum DATA))) (Assign "q" (Sum DATA) (Assign "p" (Variable "q") (Return "p"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign q = set(product(sum(DATA))); assign q = sum(DATA); assign p = DATA; return q;"), Just (Assign "q" (MakeSet (Product (Sum DATA))) (Assign "q" (Sum DATA) (Assign "p" DATA (Return "q"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign q = set(product(sum(DATA))); assign q = sum(DATA); assign p = DATA; return p;"), Just (Assign "q" (MakeSet (Product (Sum DATA))) (Assign "q" (Sum DATA) (Assign "p" DATA (Return "p"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(b); assign b = b; return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min (Variable "b")) (Assign "b" (Variable "b") (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(b); assign b = DATA; return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min (Variable "b")) (Assign "b" DATA (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = union(set(DATA)); assign a = b; return b;"), Just (Assign "b" (Union (MakeSet DATA)) (Assign "a" (Variable "b") (Return "b")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = union(set(DATA)); assign a = b; return a;"), Just (Assign "b" (Union (MakeSet DATA)) (Assign "a" (Variable "b") (Return "a")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(b); assign a = b; return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min (Variable "b")) (Assign "a" (Variable "b") (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(b); assign a = b; return a;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min (Variable "b")) (Assign "a" (Variable "b") (Return "a"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(b); assign a = DATA; return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min (Variable "b")) (Assign "a" DATA (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(b); assign a = DATA; return a;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min (Variable "b")) (Assign "a" DATA (Return "a"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Return "b")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); assign b = set(DATA); return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Assign "b" (MakeSet DATA) (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); assign b = b; return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Assign "b" (Variable "b") (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); assign b = DATA; return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Assign "b" DATA (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); assign a = set(DATA); return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Assign "a" (MakeSet DATA) (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); assign a = set(DATA); return a;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Assign "a" (MakeSet DATA) (Return "a"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = sum(min(DATA)); assign b = min(DATA); assign a = b; return a;"), Just (Assign "b" (Sum (Min DATA)) (Assign "b" (Min DATA) (Assign "a" (Variable "b") (Return "a"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = sum(min(DATA)); assign b = min(DATA); assign a = DATA; return b;"), Just (Assign "b" (Sum (Min DATA)) (Assign "b" (Min DATA) (Assign "a" DATA (Return "b"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = sum(min(DATA)); assign b = min(DATA); assign a = DATA; return a;"), Just (Assign "b" (Sum (Min DATA)) (Assign "b" (Min DATA) (Assign "a" DATA (Return "a"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = min(DATA); assign a = DATA; return a;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Min DATA) (Assign "a" DATA (Return "a"))))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = max(b); return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Max (Variable "b")) (Return "b")))),
  ((tokenizeParse :: String -> Maybe Stmt) ("assign b = min(min(DATA)); assign b = max(b); assign b = set(DATA); return b;"), Just (Assign "b" (Min (Min DATA)) (Assign "b" (Max (Variable "b")) (Assign "b" (MakeSet DATA) (Return "b")))))
  ]

typeCheckTests :: [(Maybe Type, Maybe Type)]
typeCheckTests = [
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Min (Product (Product (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Product (Max (Product (Product (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Max (Product (Product (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Product (Product (Product (Product (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Product (Product (Product (Product (Product DATA))))))), Just TyNumber),
  ((typeCheck [] :: Stmt -> Maybe Type) (Return "p"), Nothing),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Max (Product (Variable "q")))), Nothing),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "b" (Sum DATA) (Assign "a" DATA (Return "b"))), Just TyVoid),
  ((typeCheck [] :: Exp -> Maybe Type) (Sum (Max (Product (Variable "q")))), Nothing),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Product (Min (Product (Variable "q"))))), Nothing),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Min (Product (Variable "q"))))), Nothing),
  ((typeCheck [] :: Exp -> Maybe Type) (Product (Sum (Max (Product (Variable "q"))))), Nothing),
  ((typeCheck [] :: Stmt -> Maybe Type) (Assign "p" DATA (Return "p")), Just TyVoid)
  ]
interpretTests :: [(Maybe KeyValueStore, Maybe KeyValueStore)]
interpretTests = [
  (typeCheckInterpret (Assign "u" (Product (Min DATA)) (Assign "v" (Product (Min (Variable "u"))) (Return "v"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "u" (Min (Min DATA)) (Assign "v" (Min (Min (Variable "u"))) (Return "v"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "x" (Sum (Max DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [5]),([],Set [5])]),
  (typeCheckInterpret (Assign "x" (Sum (Max DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [5]),([],Set [5])]),
  (typeCheckInterpret (Assign "x" (Product (Max DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [5]),([],Set [5])]),
  (typeCheckInterpret (Assign "x" (Sum (Product DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Product (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Product (Product DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Sum (Min DATA)) (Assign "y" (Min (Min (Variable "x"))) (Return "y"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "u" (Sum (Min DATA)) (Assign "v" (Sum (Min (Variable "u"))) (Return "v"))) testKVS, Just [([],Number (-3)),([],Number (-3))]),
  (typeCheckInterpret (Assign "q" (Intersection (MakeSet DATA)) (Assign "p" (Intersection (Intersection (MakeSet (Variable "q")))) (Return "q"))) testKVS, Nothing),
  (typeCheckInterpret (Assign "x" (Product (Max DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [5]),([],Set [5])]),
  (typeCheckInterpret (Assign "x" (Min (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Min (Product DATA)) (Assign "y" (Union (Union (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Sum (Product DATA)) (Assign "y" (Intersection (Intersection (MakeSet (Variable "x")))) (Return "y"))) testKVS, Just [([],Set [-30]),([],Set [-30])]),
  (typeCheckInterpret (Assign "x" (Product (Min DATA)) (Assign "y" (Min (Min (Variable "x"))) (Return "y"))) testKVS, Just [([],Number (-1)),([],Number (-1))]),
  (typeCheckInterpret (Assign "x" (Product (Max DATA)) (Assign "y" (Max (Max (Variable "x"))) (Return "y"))) testKVS, Just [([],Number 5),([],Number 5)]),
  (typeCheckInterpret (Assign "x" (Product (Sum DATA)) (Assign "y" (Sum (Sum (Variable "x"))) (Return "y"))) testKVS, Just [([],Number 54),([],Number 54)])
  ]

testKVS = (dats [["T"], ["C", "D"], ["X", "Y", "Z"], ["a", "b"]] [-1,2,3,5]) !! 3




--eof