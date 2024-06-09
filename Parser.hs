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
parseUnary t@(Operator o) l =
  errorMessege (lookup o prefixOperatorPrecedence) "unknown prefix operator"
    >>= second ((\a -> Branch t [a]) <$>) . parseNUD l

parseFunction :: Token -> RemTokens -> ParseReturn
parseFunction f [] = Right ([], Branch f [])
parseFunction f l@(x : xs) = case x of
  StartParen -> second (Branch f) <$> parseArguments xs []
  _ -> Right (l, Branch f [])

checkParenOrCommaNext :: [TokenTree] -> (RemTokens, TokenTree) -> ErrorProne (RemTokens, [TokenTree])
checkParenOrCommaNext a r
  | null (fst r) = Left "unmatched function parenthesis"
  | otherwise = case head $ fst r of
      Comma -> parseArguments (tail (fst r)) $ a ++ [snd r]
      EndParen -> Right (tail (fst r), a ++ [snd r])
      _ -> Left "unmatched function parenthesis"

parseArguments :: RemTokens -> [TokenTree] -> ErrorProne (RemTokens, [TokenTree])
parseArguments [] a = Left "unmatched function parenthesis"
parseArguments l a = parseNUD l 0 >>= checkParenOrCommaNext a

checkParenNext :: [Token] -> ErrorProne [Token]
checkParenNext l
  | null l = Left "unmatched start parenthesis"
  | otherwise = case head l of
      EndParen -> Right $ tail l
      _ -> Left "unmatched start parenthesis"

parseParens :: RemTokens -> ParseReturn
parseParens [] = Left "unmatched start parenthesis"
parseParens l = parseNUD l 0 >>= sequenceFst . first checkParenNext

parselets :: Token -> RemTokens -> ParseReturn
parselets x xs = case x of
  Number _ -> parseNumber x xs
  Operator _ -> parseUnary x xs
  Function _ -> parseFunction x xs
  StartParen -> parseParens xs
  EndParen -> Left "unmatched end parenthesis"
  _ -> Left "invalid input"

parseNUD :: RemTokens -> Int -> ParseReturn
parseNUD [] prec = Left "empty parser input"
parseNUD (x : xs) prec = parselets x xs >>= uncurry (parseLED prec)

parseInfix :: Char -> RemTokens -> RemTokens -> TokenTree -> Int -> ParseReturn
parseInfix o lLess lMore tree prec = do
  (opPrec, opAsc) <- errorMessege (lookup o infixOperatorPrecedence) "unknown infix operator"
  nud <- parseNUD lMore opPrec
  if opPrec < prec || (opPrec == prec && opAsc)
    then Right (lLess, tree)
    else parseLED prec (fst nud) (Branch (Operator o) [tree, snd nud])

parseLED :: Int -> RemTokens -> TokenTree -> ParseReturn
parseLED prec [] tree = Right ([], tree)
parseLED prec all@(x : xs) tree = case x of
  StartParen -> parseInfix '*' all all tree prec
  Operator c -> parseInfix c all xs tree prec
  _ -> Right (all, tree)

parse :: ErrorProne [Token] -> ErrorProne TokenTree
parse (Left e) = Left e
parse (Right l) = parseNUD l 0 >>= (\p -> if null (fst p) then Right $ snd p else Left "invalid input")