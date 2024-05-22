{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}

data Token = Operator Char | Function String | Number Double | StartParen | Comma | EndParen deriving (Show, Eq)

data Tree a = Leaf a | Branch a [Tree a] deriving (Show, Eq)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch x l) = Branch (f x) $ map (fmap f) l

type TokenTree = Tree Token

type RemTokens = [Token]

type Error = String

type ParseReturn = (RemTokens, Either Error TokenTree)

-- TODO:
--  add dice rolls
--  add mod (%)
--  add postfix operators (just ! I think)
--  remove errors and change to either (oh no we're going to have to learn monads)
--  multiple Number types for integer precision? this is probably a bad idea

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

postfixOperatorPrecedence :: [(Char, Int)]
postfixOperatorPrecedence =
  [ ('!', 4)
  ]

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
parseFunction f all@(x : xs) = case x of
  StartParen -> let argsReturn = parseArguments xs [] in (fst argsReturn, do a <- sequence (snd argsReturn); Right $ Branch f a)
  _ -> (all, Right (Branch f []))

parseArguments :: RemTokens -> [Either Error TokenTree] -> (RemTokens, [Either Error TokenTree])
parseArguments [] a = ([], a ++ [Left "unmatched function parenthesis in "])
parseArguments l a
  | null (fst recPratt) = ([], [Left "unmatched function parenthesis"])
  |otherwise = case head $ fst recPratt of
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

parseInfix :: Char -> RemTokens -> RemTokens -> Either Error TokenTree -> Int -> ParseReturn
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

parseLED :: RemTokens -> Either Error TokenTree -> Int -> ParseReturn
-- parseLED l (Left e) _ = (l, Left e)
parseLED [] tree prec = ([], tree)
parseLED all@(x : xs) tree prec = case x of
  StartParen -> parseInfix '*' all all tree prec
  Operator c -> parseInfix c all xs tree prec
  _ -> (all, tree)

parse :: [Token] -> TokenTree
parse l = case snd pratt of
  (Left e) -> error e
  (Right t) -> if null (fst pratt) then t else error "invalid input"
  where
    pratt = parseNUD l 0

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

evaluate :: TokenTree -> Double
evaluate (Leaf x) = case x of
  Number n -> n
  _ -> error "bad input"
evaluate (Branch x l) = case x of
  Operator c -> applyOperator c $ map evaluate l
  Function s -> applyFunction s $ map evaluate l
  _ -> error "bad input"

calc :: String -> Double
calc = evaluate . parse . tokenize

-- main :: IO ()
main = do
  putStr "Enter Value: "
  input <- getLine
  print . calc $ input