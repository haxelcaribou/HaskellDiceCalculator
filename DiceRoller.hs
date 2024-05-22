{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

data Token = Operator Char | Function String | Number Double | StartParen | Comma | EndParen deriving (Show, Eq)

data Tree = Branch Token [Tree] | Leaf Token deriving (Show, Eq)

-- TODO:
--  add dice rolls
--  add mod (%)
--  multiple Number types for better precision? this is probably a bad idea

operatorSymbols :: [Char]
operatorSymbols = ['~', '+', '-', '*', '/', '^']

operatorLetters :: [Char]
operatorLetters = ['d', 'b', 't']

infixOperatorPrecedence :: [(Char, (Int, Bool))]
infixOperatorPrecedence =
  [ ('+', (1, True)),
    ('-', (1, True)),
    ('*', (2, True)),
    ('/', (2, True)),
    -- ('%', (2, True)),
    ('^', (4, False))
  ]

prefixOperatorPrecedence :: [(Char, Int)]
prefixOperatorPrecedence =
  [ ('+', 3),
    ('-', 3),
    ('~', 3)
  ]

getFromDict :: (Eq a) => [(a, b)] -> a -> Maybe b
getFromDict [] _ = Nothing
getFromDict (x : xs) a
  | fst x == a = Just $ snd x
  | otherwise = getFromDict xs a

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

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

tokenize :: String -> [Token]
tokenize "" = []
tokenize l@(c : cs)
  | c `elem` operatorSymbols = Operator c : tokenize cs
  | c `elem` operatorLetters && (cs == "" || head cs `notElem` letters) = Operator c : tokenize cs
  | c == '(' = StartParen : tokenize cs
  | c == ',' = Comma : tokenize cs
  | c == ')' = EndParen : tokenize cs
  | c `elem` letters = let (f, s) = pullString "" l in Function f : tokenize s
  | c `elem` '.' : digits = let (f, s) = pullDouble "" l in Number f : tokenize s
  | c == ' ' = tokenize cs
  | otherwise = []

parseNumber :: Token -> [Token] -> ([Token], Tree)
parseNumber x@(Number _) l = (l, Leaf x)
parseNumber _ _ = error "not a number"

parseUnary :: Token -> [Token] -> ([Token], Tree)
parseUnary o [] = error "invalid input"
parseUnary t@(Operator o) l =
    let opPrec = fromMaybe (error "unknown prefix operator") (getFromDict prefixOperatorPrecedence o)
        recPratt = parsePrattNUD l opPrec in (fst recPratt, Branch t [snd recPratt])

parseFunction :: Token -> [Token] -> ([Token], Tree)
parseFunction f [] = ([], Branch f [])
parseFunction f all@(x : xs) = case x of
  StartParen -> let recPratt = parseArguments xs [] in (fst recPratt, Branch f $ snd recPratt)
  _ -> (all, Branch f [])

parseArguments :: [Token] -> [Tree] -> ([Token], [Tree])
parseArguments [] a = error "unmatched parentheses"
parseArguments l a = case head $ fst recPratt of
  Comma -> parseArguments (tail (fst recPratt)) $ a ++ [snd recPratt]
  EndParen -> (tail (fst recPratt), a ++ [snd recPratt])
  _ -> error "unmatched parentheses"
  where
    recPratt = parsePrattNUD l 0

parseParens :: [Token] -> ([Token], Tree)
parseParens [] = error "unmatched parentheses"
parseParens l = case head $ fst recPratt of
  EndParen -> (tail (fst recPratt), snd recPratt)
  _ -> error "unmatched parentheses"
  where
    recPratt = parsePrattNUD l 0

parselets :: Token -> [Token] -> ([Token], Tree)
parselets x xs = case x of
  Number _ -> parseNumber x xs
  Operator _ -> parseUnary x xs
  Function _ -> parseFunction x xs
  StartParen -> parseParens xs
  _ -> error "not yet implemented"

parsePrattNUD :: [Token] -> Int -> ([Token], Tree)
parsePrattNUD [] prec = error "empty parser input"
parsePrattNUD (x : xs) prec = uncurry parsePrattLED (parselets x xs) prec

parsePrattLED :: [Token] -> Tree -> Int -> ([Token], Tree)
parsePrattLED [] tree prec = ([], tree)
parsePrattLED all@(x : xs) tree prec = case x of
  StartParen ->
    let opInfo = fromMaybe (error "unknown infix operator") (getFromDict infixOperatorPrecedence '*')
        opPrec = fst opInfo
        opAsc = snd opInfo
     in if opPrec < prec || (opPrec == prec && opAsc)
          then (all, tree)
          else
            let recPratt = parsePrattNUD all opPrec
             in parsePrattLED (fst recPratt) (Branch (Operator '*') [tree, snd recPratt]) prec
  Operator c ->
    let opInfo = fromMaybe (error "unknown infix operator") (getFromDict infixOperatorPrecedence c)
        opPrec = fst opInfo
        opAsc = snd opInfo
     in if opPrec < prec || (opPrec == prec && opAsc)
          then (all, tree)
          else
            let recPratt = parsePrattNUD xs opPrec
             in parsePrattLED (fst recPratt) (Branch x [tree, snd recPratt]) prec
  _ -> (all, tree)

parse :: [Token] -> Tree
parse x = let pratt = parsePrattNUD x 0 in if null (fst pratt) then snd pratt else error "invalid input"

fac :: Double -> Double
fac 0 = 1
fac x = x * fac (x - 1)

applyOperator :: Char -> [Double] -> Double
applyOperator _ [] = error "too few operands"
applyOperator _ (a : b : c : xs) = error "too many operands"
applyOperator o [a, b]
  | o == '+' = a + b
  | o == '-' = a - b
  | o == '*' = a * b
  | o == '/' = a / b
  | o == '^' = a ** b
  | otherwise = error "unknown operator"
applyOperator o [a]
  | o == '+' = a
  | o == '-' = -a
  | o == '~' = -a
  | o == '!' = fac a
  | otherwise = error "unknown unary operator"

applyFunction :: String -> [Double] -> Double
applyFunction o []
  | o == "pi" = pi
  | o == "e" = exp 1
  | otherwise = error "unknown constant"
applyFunction o [x]
  | o == "negate" = negate x
  | o == "abs" = abs x
  | o == "sign" = signum x
  | o == "round" = fromIntegral $ round x
  | o == "trunc" = fromIntegral $ truncate x
  | o == "floor" = fromIntegral $ floor x
  | o == "ceil" = fromIntegral $ ceiling x
  | o == "exp" = exp x
  | o == "sqrt" = sqrt x
  | o == "ln" = log x
  | o == "log" = logBase 10 x
  | o == "fac" = fac x
  | o == "sin" = sin x
  | o == "tan" = tan x
  | o == "cos" = cos x
  | o == "asin" = asin x
  | o == "atan" = atan x
  | o == "acos" = acos x
  | o == "rad" = pi / 180 * x
  | o == "deg" = 180 / pi * x
applyFunction o [a, b]
  | o == "add" = a + b
  | o == "sub" = a - b
  | o == "mult" = a * b
  | o == "div" = a / b
  | o == "pow" = a ** b
  | o == "log" = logBase b a
applyFunction o l@(x : xs)
  | o == "sum" = sum l
  | o == "prod" = product l
  | o == "min" = minimum l
  | o == "max" = maximum l
  | otherwise = error "unknown function"

evaluate :: Tree -> Double
evaluate (Leaf x) = case x of
  Number n -> n
  _ -> error "bad input"
evaluate (Branch x l) = case x of
  Operator c -> applyOperator c $ map evaluate l
  Function s -> applyFunction s $ map evaluate l
  _ -> error "bad input"

calc :: String -> Double
calc s = evaluate $ parse $ tokenize s