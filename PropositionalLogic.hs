module PropositionalLogic where

import Data.List (nub)
import Prelude hiding (showParen)

type Env = [(String, Bool)]

data Formula
  = And Formula Formula 
  | Or Formula Formula 
  | If Formula Formula
  | Iff Formula Formula 
  | Not Formula
  | Var String 

instance Show Formula where
  show (Var s)   = s
  show (Not f)   = "~" ++ show f
  show (And f g) = showParen f ++ " ^ " ++ showParen g
  show (Or f g)  = showParen f ++ " V " ++ showParen g
  show (If f g)  = showParen f ++ " -> " ++ showParen g
  show (Iff f g) = showParen f ++ " <-> " ++ showParen g
  
showParen :: Formula -> String
showParen f@(Var _) = show f
showParen f@(Not _) = show f
showParen f         = "(" ++ show f ++ ")" 
  
eval :: Formula -> Env -> Bool 
eval (Var str) env = maybe False id (lookup str env)
eval (Not f) env   = not $ eval f env 
eval (And f g) env = eval f env && eval g env
eval (Or  f g) env = eval f env || eval g env
eval (If  f g) env = not (eval f env) || eval g env
eval (Iff f g) env = eval f env == eval g env

vars :: Formula -> [String]
vars (Var str) = [str] 
vars (Not f)   = vars f  
vars (And f g) = vars f ++ vars g
vars (Or f g)  = vars f ++ vars g
vars (If f g)  = vars f ++ vars g
vars (Iff f g) = vars f ++ vars g

envs :: [String] -> [Env] 
envs [] = [[]]
envs (x:xs) = [ (x, b) : env | b <- [True, False], env <- envs xs] 

truthTable :: [Env] -> Formula -> String
truthTable envs f =
  let
    variables = nub $ vars f
    header = unwords variables ++ " | " ++ show f 
    separator = replicate (length header) '-'
    rows = [ unwords [showBool (lookupVar v env) | v <- variables] ++ " | " ++ showBool (Just (eval f env))
           | env <- envs
           ]
  in unlines (header : separator : rows)
 where
  showBool :: Maybe Bool -> String
  showBool (Just True)  = "T"
  showBool (Just False) = "F"
  showBool Nothing      = "?"

  lookupVar :: String -> Env -> Maybe Bool
  lookupVar v env = lookup v env
  
tautology :: Formula -> Bool 
tautology f = all (\env -> eval f env) (envs $ vars f)

contradiction :: Formula -> Bool 
contradiction f = all (\env -> eval f env) (envs $ vars f)

contingency :: Formula -> Bool 
contingency f = not (tautology f) && not (contradiction f) 

--main :: IO ()
--main = do 
--  let f = If (And (Var "p") (Var "q")) (Var "p")
--  putStrLn $ truthTable (envs (nub $ vars f)) f
