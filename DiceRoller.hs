{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

data Token = Operator Char | Function String | Number Double | StartParen | Comma | EndParen deriving (Show, Eq)

data Tree = Branch Token [Tree] | Leaf Token deriving (Show, Eq)

-- TODO:
--   add unary operator support

operatorSymbols :: [Char]
operatorSymbols = ['+', '-', '*', '/', '^']

operatorLetters :: [Char]
operatorLetters = ['d', 'b', 't']

-- operatorPrecedence :: (Char, (Int, Bool)) -- why does this not work???
operatorPrecedence =
  [ ('+', (1, True)),
    ('-', (1, True)),
    ('*', (2, True)),
    ('/', (2, True)),
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
parsePrattParens all@(x : xs) = case head $ fst recPratt of
  EndParen -> (tail (fst recPratt), snd recPratt)
  _ -> error "unmatched parentheses"
  where
    recPratt = parsePrattTreeNUD all 0

parsePrattArguments :: [Token] -> [Tree] -> ([Token], [Tree])
parsePrattArguments [] a = error "unmatched parentheses"
parsePrattArguments all@(x : xs) a = case head $ fst recPratt of
  Comma -> parsePrattArguments (tail (fst recPratt)) $ snd recPratt : a
  EndParen -> (tail (fst recPratt), snd recPratt : a)
  _ -> error "unmatched parentheses"
  where
    recPratt = parsePrattTreeNUD all 0

parsePrattFunction :: Token -> [Token] -> ([Token], Tree)
parsePrattFunction f [] = ([], Branch f [])
parsePrattFunction f all@(x : xs) = case x of
  StartParen -> let recPratt = parsePrattArguments xs [] in (fst recPratt, Branch f $ snd recPratt)
  _ -> (all, Branch f [])

parsePrattTreeNUD :: [Token] -> Int -> ([Token], Tree)
parsePrattTreeNUD [] prec = error "empty parser input"
parsePrattTreeNUD all@(x : xs) prec = case x of
  Number _ -> parsePrattTreeLED xs (Leaf x) prec
  Function _ -> let recPratt = parsePrattFunction x xs in uncurry parsePrattTreeLED recPratt prec
  -- Operator _ -> parsePrattTreeLED xs (Branch x []) prec
  StartParen -> let recPratt = parsePrattParens xs in uncurry parsePrattTreeLED recPratt prec
  _ -> error "not yet implemented"

parsePrattTreeLED :: [Token] -> Tree -> Int -> ([Token], Tree)
parsePrattTreeLED [] tree prec = ([], tree)
parsePrattTreeLED all@(x : xs) tree prec = case x of
  Operator c ->
    let opPrec = fst (getFromDict operatorPrecedence c)
     in if opPrec < prec || (opPrec == prec && snd (getFromDict operatorPrecedence c))
          then (all, tree)
          else
            let recPratt = parsePrattTreeNUD xs opPrec
             in parsePrattTreeLED (fst recPratt) (Branch x [tree, snd recPratt]) prec
  _ -> (all, tree)

parseTree :: [Token] -> Tree
parseTree x = snd $ parsePrattTreeNUD x 0

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
  | o == '~' = -a
  | otherwise = error "unknown unary operator"

applyFunction :: String -> [Double] -> Double
applyFunction o []
  | o == "pi" = pi
  | otherwise = error "unknown constant"
applyFunction o (x : xs)
  | o == "min" = foldl min x xs
  | o == "max" = foldl max x xs
  | o == "abs" && null xs = abs x
  | otherwise = error "unknown function"

evaluateTree :: Tree -> Double
evaluateTree (Leaf x) = case x of
  Number n -> n
  _ -> error "bad input"
evaluateTree (Branch x l) = case x of
  Operator c -> applyOperator c $ map evaluateTree l
  Function s -> applyFunction s $ map evaluateTree l
  _ -> error "bad input"

calc :: String -> Double
calc s = evaluateTree $ parseTree $ tokenize s