module Tokenizer (tokenize) where

import Error
import Token

letters :: [Char]
letters = ['a' .. 'z'] ++ ['A' .. 'Z']

digits :: [Char]
digits = ['0' .. '9']

pullDouble :: String -> String -> ErrorProne (Double, String)
pullDouble f [] = Right (read f :: Double, "")
pullDouble f l@(c : cs)
  | c == '.' && null f = pullDouble' "0." cs
  | c == '.' = pullDouble' (f ++ ".") cs
  | c `elem` digits = pullDouble (f ++ [c]) cs
  | otherwise = Right (read f :: Double, l)

pullDouble' :: String -> String -> ErrorProne (Double, String)
pullDouble' f [] = Right (read (f ++ "0") :: Double, "")
pullDouble' f l@(c : cs)
  | c == '.' = Left "multiple decimal points in number"
  | c `elem` digits = pullDouble' (f ++ [c]) cs
  | otherwise = Right (read (f ++ "0") :: Double, l)

pullString :: String -> String -> (String, String)
pullString s [] = (s, "")
pullString s l@(c : cs)
  | c `elem` letters = pullString (s ++ [c]) cs
  | otherwise = (s, l)

tokenize :: String -> ErrorProne [Token]
tokenize "" = Right []
tokenize l@(c : cs)
  | c `elem` operatorSymbols = (Operator c :) <$> tokenize cs
  | c `elem` operatorLetters && (null cs || head cs `notElem` letters) = (Operator c :) <$> tokenize cs
  | c `elem` map fst parenSymbols = (StartParen (toParen c) :) <$> tokenize cs
  | c == ',' = (Comma :) <$> tokenize cs
  | c `elem` map snd parenSymbols = (EndParen (toParen c) :) <$> tokenize cs
  | c `elem` letters = let (f, s) = pullString "" l in (Function f :) <$> tokenize s
  | c `elem` '.' : digits = do
      (d, s) <- pullDouble "" l
      (Number d :) <$> tokenize s
  | c == ' ' = tokenize cs
  | otherwise = Left $ "unrecognized symbol '" ++ [c] ++ "'"
