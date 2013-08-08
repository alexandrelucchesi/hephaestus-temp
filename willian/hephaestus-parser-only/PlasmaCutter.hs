module PlasmaCutter where

import BasicTypes
import LineSintaticScanner
import BoolExprSintaticScanner

extractCode :: [String] -> Environment -> [String]
extractCode [] _ = []
extractCode (x:xs) env = extr ++ (extractCode xs newEnv)
 where
  extr = [snd $ extractLine (parseLine x) env]
  newEnv = fst $ extractLine (parseLine x) env

extractLine :: Line -> Environment -> (Environment, String)
extractLine (Condition line) (var,st) = ((var, newSt), [])
 where
  evRes = evaluateCondition line var
  newSt = [evRes] ++ st
  
extractLine (EndCondition) (var,st) = ((var, tail st), [])
extractLine (CodeLine line) env@(_,st) = if head st then (env, line) else (env, [])

evaluateCondition :: String -> Variables -> Bool
evaluateCondition fun env = evalFunction env $ parseFunction fun

evalFunction :: Variables -> Expression -> Bool
evalFunction env (And lhs rhs) = (evalFunction env lhs) && (evalFunction env rhs)
evalFunction env (Or lhs rhs) = (evalFunction env lhs) || (evalFunction env rhs)
evalFunction env (Not exp) = not (evalFunction env exp)
evalFunction env (ExpId var) =   
  let exp = [snd x | x <- env, var == fst x] 
  in case exp of 
       []  -> error (var ++ " not in scope.")
       [x] -> x
