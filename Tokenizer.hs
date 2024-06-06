module Tokenizer (tokenize) where
import Token
import Error

letters :: [Char]
letters = ['a' .. 'z'] ++ ['A' .. 'Z']

digits :: [Char]
digits = ['0' .. '9']

pullDouble :: String -> String -> (Double, String)
pullDouble f [] = (read f :: Double, "")
pullDouble f l@(c : cs)
  | c `elem` '.' : digits = pullDouble (f ++ [c]) cs
  | otherwise = (read f :: Double, l)

pullString :: String -> String -> (String, String)
pullString s [] = (s, "")
pullString s l@(c : cs)
  | c `elem` letters = pullString (s ++ [c]) cs
  | otherwise = (s, l)

tokenize :: String -> ErrorProne [Token]
tokenize "" = Right []
tokenize l@(c : cs)
  | c `elem` operatorSymbols = (Operator c :) <$> tokenize cs
  | c `elem` operatorLetters && (cs == "" || head cs `notElem` letters) = (Operator c :) <$> tokenize cs
  | c == '(' = (StartParen :) <$> tokenize cs
  | c == ',' = (Comma :) <$> tokenize cs
  | c == ')' = (EndParen :) <$> tokenize cs
  | c `elem` letters = let (f, s) = pullString "" l in (Function f :) <$> tokenize s
  | c `elem` '.' : digits = let (f, s) = pullDouble "" l in (Number f :) <$> tokenize s
  | c == ' ' = tokenize cs
  | otherwise = Left "unrecognized token"
