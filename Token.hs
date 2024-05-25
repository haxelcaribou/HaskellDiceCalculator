module Token (Token (Operator, Function, Number, StartParen, Comma, EndParen), operatorSymbols, operatorLetters) where

data Token = Operator Char | Function String | Number Double | StartParen | Comma | EndParen deriving (Show, Eq)

operatorSymbols :: [Char]
operatorSymbols = ['~', '+', '-', '*', '/', '^']

operatorLetters :: [Char]
operatorLetters = ['d', 'b', 't']
