{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}

module Parser (parse) where

import Error
import Token
import Tree

type RemTokens = [Token]

type TokenTree = Tree Token

type ParseReturn = (RemTokens, ErrorProne TokenTree)

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

infixOperatorPrecedence :: [(Char, (Int, Bool))]
infixOperatorPrecedence =
  [ ('+', (1, True)),
    ('-', (1, True)),
    ('*', (2, True)),
    ('/', (2, True)),
    -- ('%', (2, True)),
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
  [ ('!', 4)
  ]

parseNumber :: Token -> RemTokens -> ParseReturn
parseNumber x@(Number _) l = (l, Right (Leaf x))
parseNumber _ _ = ([], Left "not a number")

parseUnary :: Token -> RemTokens -> ParseReturn
parseUnary o [] = ([], Left "invalid input")
parseUnary t@(Operator o) l =
  let opPrec = fromMaybe (error "unknown prefix operator") (lookup o prefixOperatorPrecedence)
      recPratt = parseNUD l opPrec
   in (fst recPratt, do a <- snd recPratt; Right $ Branch t [a])

parseFunction :: Token -> RemTokens -> ParseReturn
parseFunction f [] = ([], Right (Branch f []))
parseFunction f l@(x : xs) = case x of
  StartParen -> let argsReturn = parseArguments xs [] in (fst argsReturn, do a <- sequence (snd argsReturn); Right $ Branch f a)
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
      EndParen -> (tail (fst recPratt), snd recPratt)
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
parseInfix o lLess lMore tree prec
  | opPrec < prec || (opPrec == prec && opAsc) = (lLess, tree)
  | otherwise =
      let recPratt = parseNUD lMore opPrec
       in parseLED
            (fst recPratt)
            ( do
                a <- tree
                b <- snd recPratt
                Right (Branch (Operator o) [a, b])
            )
            prec
  where
    opInfo = fromMaybe (error "unknown infix operator") (lookup o infixOperatorPrecedence)
    opPrec = fst opInfo
    opAsc = snd opInfo

parseLED :: RemTokens -> ErrorProne TokenTree -> Int -> ParseReturn
-- parseLED l (Left e) _ = (l, Left e)
parseLED [] tree prec = ([], tree)
parseLED all@(x : xs) tree prec = case x of
  StartParen -> parseInfix '*' all all tree prec
  Operator c -> parseInfix c all xs tree prec
  _ -> (all, tree)

parse :: [Token] -> ErrorProne TokenTree
parse l
  | null (fst pratt) = snd pratt
  | otherwise = Left "invalid input"
  where
    pratt = parseNUD l 0