{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

data Token = Operator Char | Function String | Number Double | StartParen | Comma | EndParen deriving (Show, Eq)

data Tree = Branch Token [Tree] | Leaf Token deriving (Show, Eq)

-- TODO:
--  add dice rolls
--  add mod (%)
--  add unary operator support
--  multiple Number types for better precision? this is probably a bad idea

operatorSymbols :: [Char]
operatorSymbols = ['+', '-', '*', '/', '^']

operatorLetters :: [Char]
operatorLetters = ['d', 'b', 't']

operatorPrecedence :: [(Char, (Int, Bool))]
operatorPrecedence =
  [ ('+', (1, True)),
    ('-', (1, True)),
    ('*', (2, True)),
    ('/', (2, True)),
    -- ('%', (2, True)),
    ('^', (3, False)),
    (' ', (0, False))
  ]

getFromDict :: (Eq a) => [(a, b)] -> a -> b
getFromDict (x : xs) a
  | fst x == a = snd x
  | null xs = snd x
  | otherwise = getFromDict xs a

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

parsePrattParens :: [Token] -> ([Token], Tree)
parsePrattParens [] = error "unmatched parentheses"
parsePrattParens l = case head $ fst recPratt of
  EndParen -> (tail (fst recPratt), snd recPratt)
  _ -> error "unmatched parentheses"
  where
    recPratt = parsePrattNUD l 0

parsePrattArguments :: [Token] -> [Tree] -> ([Token], [Tree])
parsePrattArguments [] a = error "unmatched parentheses"
parsePrattArguments l a = case head $ fst recPratt of
  Comma -> parsePrattArguments (tail (fst recPratt)) $ a ++ [snd recPratt]
  EndParen -> (tail (fst recPratt), a ++ [snd recPratt])
  _ -> error "unmatched parentheses"
  where
    recPratt = parsePrattNUD l 0

parsePrattFunction :: Token -> [Token] -> ([Token], Tree)
parsePrattFunction f [] = ([], Branch f [])
parsePrattFunction f all@(x : xs) = case x of
  StartParen -> let recPratt = parsePrattArguments xs [] in (fst recPratt, Branch f $ snd recPratt)
  _ -> (all, Branch f [])

parsePrattNUD :: [Token] -> Int -> ([Token], Tree)
parsePrattNUD [] prec = error "empty parser input"
parsePrattNUD (x : xs) prec = case x of
  Number _ -> parsePrattLED xs (Leaf x) prec
  Function _ -> let recPratt = parsePrattFunction x xs in uncurry parsePrattLED recPratt prec
  -- Operator _ -> parsePrattTreeLED xs (Branch x []) prec
  StartParen -> let recPratt = parsePrattParens xs in uncurry parsePrattLED recPratt prec
  _ -> error "not yet implemented"

parsePrattLED :: [Token] -> Tree -> Int -> ([Token], Tree)
parsePrattLED [] tree prec = ([], tree)
parsePrattLED all@(x : xs) tree prec = case x of
  Operator c ->
    let opPrec = fst (getFromDict operatorPrecedence c)
     in if opPrec < prec || (opPrec == prec && snd (getFromDict operatorPrecedence c))
          then (all, tree)
          else
            let recPratt = parsePrattNUD xs opPrec
             in parsePrattLED (fst recPratt) (Branch x [tree, snd recPratt]) prec
  _ -> (all, tree)

parse :: [Token] -> Tree
parse x = snd $ parsePrattNUD x 0

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