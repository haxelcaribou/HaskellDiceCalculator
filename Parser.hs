module Parser (parse) where

import Data.Bifunctor (first, second)
import Error
import Token
import Tree

type RemTokens = [Token]

type TokenTree = Tree Token

type ParseReturn = ErrorProne (RemTokens, TokenTree)

sequenceSnd :: (a, ErrorProne b) -> ErrorProne (a, b)
sequenceSnd (_, Left e) = Left e
sequenceSnd (a, Right b) = Right (a, b)

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

errorMessege :: Maybe a -> String -> ErrorProne a
errorMessege Nothing e = Left e
errorMessege (Just x) _ = Right x

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

parseArguments :: RemTokens -> [ErrorProne TokenTree] -> ErrorProne (RemTokens, [TokenTree])
parseArguments [] a = Left "unmatched function parenthesis"
parseArguments l a = case recPratt' of
  Left e -> Left e
  Right recPratt ->
    if null (fst recPratt)
      then Left "unmatched function parenthesis"
      else case head $ fst recPratt of
        Comma -> parseArguments (tail (fst recPratt)) $ a ++ [Right (snd recPratt)]
        EndParen -> (\x -> (tail (fst recPratt), x ++ [snd recPratt])) <$> sequence a
        _ -> Left "unmatched function parenthesis"
  where
    recPratt' = parseNUD l 0

parseParens :: RemTokens -> ParseReturn
parseParens [] = Left "unmatched start parenthesis"
parseParens l = case recPratt' of
  Left e -> Left e
  Right recPratt ->
    if null (fst recPratt)
      then Left "unmatched start parenthesis"
      else case head $ fst recPratt of
        EndParen -> first tail (Right recPratt)
        _ -> Left "unmatched start parenthesis"
  where
    recPratt' = parseNUD l 0

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
parseNUD (x : xs) prec = parselets x xs >>= (\a -> uncurry parseLED a prec)

parseInfix :: Char -> RemTokens -> RemTokens -> TokenTree -> Int -> ParseReturn
-- parseInfix _ l _ (Left e) _ = (l, Left e)
parseInfix o lLess lMore tree prec =
  errorMessege (lookup o infixOperatorPrecedence) "unknown infix operator"
    >>= uncurry (parseInfix' o lLess lMore tree prec)

parseInfix' :: Char -> RemTokens -> RemTokens -> TokenTree -> Int -> Int -> Bool -> Either Error (RemTokens, TokenTree)
parseInfix' o lLess lMore tree prec opPrec opAsc =
  parseNUD lMore opPrec
    >>= ( \r ->
            if opPrec < prec || (opPrec == prec && opAsc)
              then Right (lLess, tree)
              else parseLED (fst r) (Branch (Operator o) [tree, snd r]) prec
        )

parseLED :: RemTokens -> TokenTree -> Int -> ParseReturn
-- parseLED l (Left e) _ = (l, Left e)
parseLED [] tree prec = Right ([], tree)
parseLED all@(x : xs) tree prec = case x of
  StartParen -> parseInfix '*' all all tree prec
  Operator c -> parseInfix c all xs tree prec
  _ -> Right (all, tree)

parse :: ErrorProne [Token] -> ErrorProne TokenTree
parse (Left e) = Left e
parse (Right l) = case pratt' of
  Left e -> Left e
  Right pratt ->
    if null (fst pratt)
      then Right $ snd pratt
      else Left "invalid input"
  where
    pratt' = parseNUD l 0