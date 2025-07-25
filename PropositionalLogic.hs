data Formula
  = And Formula Formula 
  | Or Formula Formula 
  | If Formula Formula
  | Iff Formula Formula 
  | Not Formula
  | Val Bool 
 deriving (Show)
  
eval :: Formula -> Bool 
eval (Val x)   = x
eval (Not x)   = not $ eval x 
eval (And x y) = and [eval x, eval y] 
eval (Or x y)  = or [eval x, eval y] 
eval (If x y)  = or [not (eval x), eval y] 
eval (Iff x y) = or [and [eval x, eval y], and [not (eval x), not (eval y)]]

variants :: Formula -> [Formula]
variants (Val _)   = [Val True, Val False]
variants (Not f)   = [Not f' | f' <- variants f]
variants (And x y) = [And x' y' | x' <- variants x, y' <- variants y]
variants (Or x y)  = [Or x' y'  | x' <- variants x, y' <- variants y]
variants (If x y)  = [If x' y'  | x' <- variants x, y' <- variants y]
variants (Iff x y) = [Iff x' y' | x' <- variants x, y' <- variants y]

tautology :: [Formula] -> Bool
tautology = all eval

logicallyEquivalent :: Formula -> Formula -> Bool 
logicallyEquivalent f g = (eval' f) == (eval' g) 
  where 
    eval' :: Formula -> [Bool]
    eval' f = map eval (variants f) 
  
main :: IO ()
main = do 
  let f = And (And (Val True) (Val True)) (Val True)
  let g = And (Val True) (And (Val True) (Val True))
  print $ logicallyEquivalent f g 
