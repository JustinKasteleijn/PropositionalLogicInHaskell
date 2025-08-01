module Parser where

import Control.Applicative (Alternative(..))
import Data.Char (isSpace, isDigit, isAlphaNum, isAlpha)

newtype Parser a = Parser { parse :: String -> Either String (a, String) }

-- Instances
instance Functor Parser where 
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = 
    Parser $ \input -> do
      (x, rest) <- parse px input 
      Right (f x, rest)

instance Applicative Parser where 
  pure :: a -> Parser a
  pure x = 
    Parser $ \input -> 
      Right (x, input)
      
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = 
    Parser $ \input -> do
      (f, rest)  <- parse pf input
      (x, rest') <- parse px rest 
      Right (f x, rest')

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = 
    Parser $ \input -> do 
      (x, rest) <- parse px input 
      parse (f x) rest
      
instance MonadFail Parser where 
  fail :: String -> Parser a
  fail _ = empty
      
instance Alternative Parser where 
  empty :: Parser a 
  empty = 
    Parser $ \_ -> Left "" 
    
  (<|>) :: Parser a -> Parser a -> Parser a 
  px <|> py = 
    Parser $ \input -> 
      case parse px input of 
        Right r   -> Right r
        Left err1 -> case parse py input of
          Right r   -> Right r
          Left err2 -> Left (err1 ++ "AND" ++ err2) 

  some :: Parser a -> Parser [a]
  some p = some'
    where 
      many' = some' <|> pure []
      some' = (:) <$> p <*> many'
  
  many :: Parser a -> Parser [a]
  many p = many'
    where 
      many' = some' <|> pure []
      some' = (:) <$> p <*> many'

-- Parser Functions 
item :: Parser Char 
item = 
  Parser $ \input -> 
    case input of 
        []     -> Left "Expected anything but got end of input"
        (x:xs) -> Right (x, xs)

char :: Char -> Parser Char 
char c = Parser charP
  where 
    charP [] = Left "Expected character but got end of input"
    charP (x:xs)
      | x == c    = Right (c, xs)
      | otherwise = Left $ "Expected '" ++ [c] ++ "' but got '" ++ [x] ++ "'"


space :: Parser Char
space = sat (\c -> c == ' ' || c == '\t')      

digit :: Parser Char 
digit = sat isDigit

alphaNum :: Parser Char 
alphaNum = sat isAlphaNum

alpha :: Parser Char
alpha = sat isAlpha

newline :: Parser ()
newline = (char '\n'
  <|> char '\r')
  *> pure ()

token :: Parser a -> Parser a
token p = whitespace *> p <* whitespace

nat :: Parser Int 
nat = read <$> some digit

int :: Parser Int 
int = nat <|> char '-' *> (negate <$> nat)
    
string :: String -> Parser String 
string = mapM char

indented :: Parser ()
indented = string "  " *> pure ()

sat :: (Char -> Bool) -> Parser Char
sat pred = 
  Parser $ \input -> do
    (x, rest) <- parse item input
    if pred x
      then Right (x, rest)
      else Left "Does not satisfy predicate"

whitespace :: Parser () 
whitespace = 
  many space *> pure ()
  
lineBreak :: Parser ()
lineBreak = many space *> newline *> pure ()
  
sepBy1 :: Parser a -> Parser b -> Parser [a] 
sepBy1 px psep = (:) 
  <$> px 
  <*> many (psep *> px) 

sepBy :: Parser a -> Parser b -> Parser [a] 
sepBy px psep = sepBy1 px psep <|> pure []

try :: Parser a -> Parser a
try p = Parser $ \input ->
  case parse p input of
    Left err -> Left err
    success  -> success
      
between :: Parser open -> Parser close -> Parser a -> Parser a 
between po pc p = 
   po 
   *> p
   <* pc 
   
optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing 

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= rest 
  where 
    rest x = (do
      f <- op 
      y <- p 
      rest (f x y)) <|> pure x
