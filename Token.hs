module Token (Token (Operator, Function, Number, StartParen, Comma, EndParen), Paren, toParen, operatorSymbols, operatorLetters, parenSymbols) where

data Paren = RoundBracket | SquareBracket | CurlyBrace deriving (Show, Eq)

toParen :: Char -> Paren
toParen c
  | c `elem` ['(', ')'] = RoundBracket
  | c `elem` ['[', ']'] = SquareBracket
  | c `elem` ['{', '}'] = CurlyBrace
  | otherwise = error "not a parenthesis"

data Token = Operator Char | Function String | Number Double | StartParen Paren | Comma | EndParen Paren deriving (Show, Eq)

operatorSymbols :: [Char]
operatorSymbols = ['~', '+', '-', '*', '/', '%', '^', '!']

operatorLetters :: [Char]
operatorLetters = ['d', 'b', 't']

parenSymbols :: [(Char, Char)]
parenSymbols = [('(', ')'), ('[', ']'), ('{', '}')]