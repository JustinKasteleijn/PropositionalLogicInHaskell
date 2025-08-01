import Parser 
import PropositionalLogic
import Control.Applicative (Alternative(..))
import Data.List (nub)

parseFormula :: Parser Formula 
parseFormula = parseIff 
  
parseIff :: Parser Formula 
parseIff = chainl parseIf iffOp
  where 
    iffOp = whitespace 
            *> string "<->" 
            *> whitespace 
            *> pure Iff
  
parseIf :: Parser Formula 
parseIf = chainl parseOr ifOp
  where 
    ifOp = whitespace 
           *> string "->" 
           *> whitespace 
           *> pure If 
  
parseOr :: Parser Formula 
parseOr = chainl parseAnd orOp
  where 
    orOp = whitespace 
           *> char 'V' 
           *> whitespace 
           *> pure Or
  
parseAnd :: Parser Formula 
parseAnd = chainl parseNot andOp
  where 
    andOp = whitespace 
            *> char '^'
            *> whitespace
            *> pure And

parseNot :: Parser Formula 
parseNot = Not 
  <$> (char '~' *> whitespace *> parseNot)
  <|> parseVar

parseVar :: Parser Formula 
parseVar = Var <$> some alpha

main :: IO ()
main = do 
  let f = parse parseFormula "~p V q"
  putStrLn $ case f of 
    Left s  -> s
    Right f' -> let f'' = fst f'
                in truthTable (envs (nub $ vars f'')) f''
