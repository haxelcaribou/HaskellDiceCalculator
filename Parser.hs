module Parser (parse) where

import Data.Bifunctor (first, second)
import Error
import Token
import Tree

type RemTokens = [Token]

type TokenTree = Tree Token

type ParseReturn = ErrorProne (RemTokens, TokenTree)

sequenceFst :: (ErrorProne a, b) -> ErrorProne (a, b)
sequenceFst (Left e, _) = Left e
sequenceFst (Right a, b) = Right (a, b)

sequenceSnd :: (a, ErrorProne b) -> ErrorProne (a, b)
sequenceSnd (_, Left e) = Left e
sequenceSnd (a, Right b) = Right (a, b)

errorMessege :: Maybe a -> String -> ErrorProne a
errorMessege m s = maybe (Left s) Right m

infixOperatorPrecedence :: [(Char, (Int, Bool))]
infixOperatorPrecedence =
  [ ('+', (1, True)),
    ('-', (1, True)),
    ('*', (2, True)),
    ('/', (2, True)),
    ('%', (2, True)),
    ('^', (4, False)),
    ('d', (5, True))
  ]

ternaryOperators :: [(Char, ([Char], Bool))]
ternaryOperators =
  [ ('d', (['t', 'b'], True))
  ]

prefixOperatorPrecedence :: [(Char, Int)]
prefixOperatorPrecedence =
  [ ('+', 3),
    ('-', 3),
    ('~', 3),
    ('d', 5)
  ]

postfixOperatorPrecedence :: [(Char, Int)]
postfixOperatorPrecedence =
  [('!', 4)]

parseNumber :: Token -> RemTokens -> ParseReturn
parseNumber x@(Number _) l = Right (l, Leaf x)
parseNumber _ _ = Left "not a number"

parseUnary :: Token -> RemTokens -> ParseReturn
parseUnary o [] = Left "invalid input"
parseUnary t@(Operator o) l = do
  x <- errorMessege (lookup o prefixOperatorPrecedence) ("unknown prefix operator '" ++ [o] ++ "'")
  second (\a -> Branch t [a]) <$> parseNUD l x

parseFunction :: Token -> RemTokens -> ParseReturn
parseFunction f [] = Right ([], Branch f [])
parseFunction f l@(x : xs) = case x of
  StartParen sp -> second (Branch f) <$> parseArguments sp xs []
  _ -> Right (l, Branch f [])

checkParenOrCommaNext :: Paren -> [TokenTree] -> (RemTokens, TokenTree) -> ErrorProne (RemTokens, [TokenTree])
checkParenOrCommaNext sp l (rem, tree)
  | null rem = Left "unmatched function parenthesis"
  | otherwise = case head rem of
      Comma -> parseArguments sp (tail rem) $ l ++ [tree]
      EndParen ep ->
        if sp == ep
          then Right (tail rem, l ++ [tree])
          else Left "parenthesis types do not match"
      _ -> Left "unmatched function parenthesis"

parseArguments :: Paren -> RemTokens -> [TokenTree] -> ErrorProne (RemTokens, [TokenTree])
parseArguments sp [] a = Left "unmatched function parenthesis"
parseArguments sp l a = parseNUD l 0 >>= checkParenOrCommaNext sp a

checkParenNext :: Paren -> [Token] -> ErrorProne [Token]
checkParenNext sp l
  | null l = Left "unmatched start parenthesis"
  | otherwise = case head l of
      EndParen ep ->
        if sp == ep
          then Right $ tail l
          else Left "parenthesis types do not match"
      _ -> Left "unmatched start parenthesis"

parseParens :: Paren -> RemTokens -> ParseReturn
parseParens sp [] = Left "unmatched start parenthesis"
parseParens sp l = parseNUD l 0 >>= sequenceFst . first (checkParenNext sp)

parselets :: Token -> RemTokens -> ParseReturn
parselets x xs = case x of
  Number _ -> parseNumber x xs
  Operator _ -> parseUnary x xs
  Function _ -> parseFunction x xs
  StartParen sp -> parseParens sp xs
  EndParen _ -> Left "unmatched end parentheses or empty parenthesis" 
  _ -> Left "invalid input"

parseNUD :: RemTokens -> Int -> ParseReturn
parseNUD [] prec = Left "empty parser input"
parseNUD (x : xs) prec = parselets x xs >>= uncurry (parseLED prec)

parseTernary :: Char -> RemTokens -> TokenTree -> Int -> ParseReturn
parseTernary o l@(x : xs) tree1 prec = do
  (opPrec, opAsc) <- errorMessege (lookup o infixOperatorPrecedence) ("unknown ternary operator '" ++ [o] ++ "'")
  if opPrec < prec || (opPrec == prec && opAsc)
    then Right (l, tree1)
    else do
      (sndOps, canSolo) <- errorMessege (lookup o ternaryOperators) ("unknown ternary operator '" ++ [o] ++ "'")
      (rem2, tree2) <- parseNUD xs opPrec
      if null rem2 || head rem2 `notElem` map Operator sndOps
        then
          if canSolo
            then parseLED prec rem2 (Branch x [tree1, tree2])
            else Left $ "unmatched ternary operator '" ++ [o] ++ "'"
        else do
          (rem3, tree3) <- parseNUD (tail rem2) opPrec
          parseLED prec rem3 (Branch (head rem2) [tree1, tree2, tree3])

parseInfix :: Char -> RemTokens -> TokenTree -> Int -> ParseReturn
parseInfix o l@(x : xs) t p = do
  (opPrec, opAsc) <- errorMessege (lookup o infixOperatorPrecedence) ("unknown infix operator '" ++ [o] ++ "'")
  if opPrec < p || (opPrec == p && opAsc)
    then Right (l, t)
    else do
      (rem, tree') <- parseNUD xs opPrec
      parseLED p rem (Branch x [t, tree'])

parsePostfix :: Char -> RemTokens -> TokenTree -> Int -> ParseReturn
parsePostfix o l@(x : xs) t p = do
  opPrec <- errorMessege (lookup o postfixOperatorPrecedence) ("unknown postfix operator '" ++ [o] ++ "'")
  if opPrec < p
    then Right (l, t)
    else do
      parseLED p xs (Branch x [t])

parseOperator :: Char -> RemTokens -> TokenTree -> Int -> ParseReturn
parseOperator o l t p
  | o `elem` map fst ternaryOperators = parseTernary o l t p
  | o `elem` map fst infixOperatorPrecedence = parseInfix o l t p
  | o `elem` map fst postfixOperatorPrecedence = parsePostfix o l t p
  | otherwise = Right (l, t)

parseLED :: Int -> RemTokens -> TokenTree -> ParseReturn
parseLED p [] t = Right ([], t)
parseLED p l@(x : xs) t = case x of
  StartParen _ -> parseInfix '*' (Operator '*' : l) t p
  Operator c -> parseOperator c l t p
  _ -> Right (l, t)

parse :: ErrorProne [Token] -> ErrorProne TokenTree
parse (Left e) = Left e
parse (Right l) = do
  p <- parseNUD l 0
  if null (fst p)
    then Right $ snd p
    else case head (fst p) of
      Operator c -> Left $ "unknown operator '" ++ [c] ++ "'"
      EndParen _ -> Left "unmatched end parentheses" 
      _ -> Left "invalid input"