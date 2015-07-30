module Parse where

import AbstractSyntax

type Token = String

tokenize :: String -> [Token]
tokenize s =
  let splits = [0] ++ concat [[i,i+1] | i <- [0..length s-1], s!!i `elem` ";(), "] ++ [length s]
      all = [ [s!!i | i <- [splits!!k..(splits!!(k+1)-1)]] | k <- [0..length splits-2] ]
  in [token | token <- all, token /= " " && token /= ""]

class Parseable a where
  parse :: [Token] -> Maybe (a, [Token])

instance Parseable Exp where
  parse (t:ts) =
    if t == "max" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Max e, tail ts)

    else if t == "min" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Min e, tail ts)

    else if t == "sum" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Sum e, tail ts)

    else if t == "product" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Product e, tail ts)

    else if t == "union" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Union e, tail ts)

    else if t == "intersection" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (Intersection e, tail ts)

    else if t == "set" && length ts > 0 && ts!!0 == "(" then
      let r = parse (tail ts)
      in if r == Nothing then Nothing else
          let Just (e, ts) = r
          in if length ts == 0 || ts!!0 /= ")" then Nothing else
              Just (MakeSet e, tail ts)
    
    else if t == "DATA" then
      Just (DATA, ts)

    else if subset t "abcdefghijklmnopqrstuvwxyz" then
      Just (Variable t, ts)
    else
      Nothing
  parse _ = Nothing  

-- assign x = e ; s   |  return x ;

instance Parseable Stmt where
  parse (t:t':ts) =
    if t == "return" && t' `subset` "abcdefghijklmnopqrstuvwxyz" && ts == [";"] then
      Just (Return t', [])

    else if t == "assign" && t' `subset` "abcdefghijklmnopqrstuvwxyz" && ts!!0 == "=" then
      let r = parse(tail ts)
      in if r == Nothing then Nothing else
        let Just (e, ts) = r
        in if length ts > 0 && ts!!0 /= ";" then Nothing else
          let r = parse(tail ts)
          in if r == Nothing then Nothing else
            let Just (s, ts) = r 
            in if length ts /= 0 then Nothing else 
              Just (Assign t' e s, [])
    else Nothing

  parse _ = Nothing
      
tokenizeParse :: (Eq a, Parseable a) => String -> Maybe a
tokenizeParse s =
  let r = parse (tokenize s)
  in if r == Nothing then Nothing else
    let Just (x, ts) = r
    in if length ts > 0 then Nothing else Just x

-- Useful helper functions.
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = and [x `elem` ys | x <- xs]

-- Example of a concrete syntax string being parsed.
example = fst $ (\(Just x)->x) $ parse (tokenize "assign x = max(DATA); assign y = intersection(set(x)); return y;") :: Stmt

-- Complete the following definition for Problem 1, part (a).
kindOfParser = "predictive" -- "backtracking" or "predictive"

--eof