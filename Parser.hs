module Parser (parse) where

import Error
import Token
import Tree
import Data.Bifunctor (first, second)

type RemTokens = [Token]

type TokenTree = Tree Token

type ParseReturn = (RemTokens, ErrorProne TokenTree)

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
    ('~', 3)
  ]

postfixOperatorPrecedence :: [(Char, Int)]
postfixOperatorPrecedence =
  [('!', 4)]

parseNumber :: Token -> RemTokens -> ParseReturn
parseNumber x@(Number _) l = (l, Right (Leaf x))
parseNumber _ _ = ([], Left "not a number")

parseUnary :: Token -> RemTokens -> ParseReturn
parseUnary o [] = ([], Left "invalid input")
parseUnary t@(Operator o) l =
  let opPrec' = lookup o prefixOperatorPrecedence
   in case opPrec' of
        Nothing -> (l, Left "unknown prefix operator")
        Just opPrec ->
          let recPratt = parseNUD l opPrec
           in second ((\ a -> Branch t [a]) <$>) recPratt

parseFunction :: Token -> RemTokens -> ParseReturn
parseFunction f [] = ([], Right (Branch f []))
parseFunction f l@(x : xs) = case x of
  StartParen -> let argsReturn = parseArguments xs [] in (fst argsReturn, Branch f <$> sequence (snd argsReturn))
  _ -> (l, Right (Branch f []))

parseArguments :: RemTokens -> [ErrorProne TokenTree] -> (RemTokens, [ErrorProne TokenTree])
parseArguments [] a = ([], a ++ [Left "unmatched function parenthesis in "])
parseArguments l a
  | null (fst recPratt) = ([], [Left "unmatched function parenthesis"])
  | otherwise = case head $ fst recPratt of
      Comma -> parseArguments (tail (fst recPratt)) $ a ++ [snd recPratt]
      EndParen -> (tail (fst recPratt), a ++ [snd recPratt])
      _ -> ([], a ++ [Left "unmatched function parenthesis"])
  where
    recPratt = parseNUD l 0

parseParens :: RemTokens -> ParseReturn
parseParens [] = ([], Left "unmatched start parenthesis")
parseParens l
  | null (fst recPratt) = ([], Left "unmatched start parenthesis")
  | otherwise = case head $ fst recPratt of
      EndParen -> first tail recPratt
      _ -> ([], Left "unmatched start parenthesis")
  where
    recPratt = parseNUD l 0

parselets :: Token -> RemTokens -> ParseReturn
parselets x xs = case x of
  Number _ -> parseNumber x xs
  Operator _ -> parseUnary x xs
  Function _ -> parseFunction x xs
  StartParen -> parseParens xs
  EndParen -> ([], Left "unmatched end parenthesis")
  _ -> ([], Left "invalid input")

parseNUD :: RemTokens -> Int -> ParseReturn
parseNUD [] prec = ([], Left "empty parser input")
parseNUD (x : xs) prec = uncurry parseLED (parselets x xs) prec

parseInfix :: Char -> RemTokens -> RemTokens -> ErrorProne TokenTree -> Int -> ParseReturn
-- parseInfix _ l _ (Left e) _ = (l, Left e)
parseInfix o lLess lMore tree prec =
  let opInfoMaybe = lookup o infixOperatorPrecedence
   in case opInfoMaybe of
        Nothing -> (lLess, Left "unknown infix operator")
        Just opInfo ->
          let opPrec = fst opInfo
              opAsc = snd opInfo
              recPratt = parseNUD lMore opPrec
           in if opPrec < prec || (opPrec == prec && opAsc)
                then (lLess, tree)
                else
                  parseLED
                    (fst recPratt)
                    ( do
                        a <- tree
                        b <- snd recPratt
                        Right (Branch (Operator o) [a, b])
                    )
                    prec

parseLED :: RemTokens -> ErrorProne TokenTree -> Int -> ParseReturn
-- parseLED l (Left e) _ = (l, Left e)
parseLED [] tree prec = ([], tree)
parseLED all@(x : xs) tree prec = case x of
  StartParen -> parseInfix '*' all all tree prec
  Operator c -> parseInfix c all xs tree prec
  _ -> (all, tree)

parse :: ErrorProne [Token] -> ErrorProne TokenTree
parse (Left e) = Left e
parse (Right l)
  | null (fst pratt) = snd pratt
  | otherwise = Left "invalid input"
  where
    pratt = parseNUD l 0