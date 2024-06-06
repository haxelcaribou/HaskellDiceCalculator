module Evaluator (evaluate) where

import Dice
import Error
import System.Random
import Token
import Tree

errorMessege :: Maybe a -> String -> ErrorProne a
errorMessege Nothing e = Left e
errorMessege (Just x) _ = Right x

toIntegral :: (RealFrac a, Integral b) => a -> Maybe b
toIntegral r
  | fromIntegral i == r = Just i
  | otherwise = Nothing
  where
    i = truncate r

intFuncSingle' :: (Integral a) => (a -> Maybe a) -> (Double -> Maybe Double)
intFuncSingle' f x = fromIntegral <$> (toIntegral x >>= f)

intFuncDouble :: (Integral a) => (a -> a -> a) -> (Double -> Double -> Maybe Double)
intFuncDouble f a b = fromIntegral <$> ((f <$> toIntegral a) <*> toIntegral b)

fac :: Integral a => a -> Maybe a
fac x
  | x < 0 = Nothing
  | x == 0 = Just 1
  | otherwise = (x *) <$> fac (x - 1)

applyOperator :: StdGen -> Char -> [Double] -> ErrorProne Double
applyOperator gen _ [] = Left "too few operands"
applyOperator gen _ (a : b : c : xs) = Left "too many operands"
applyOperator gen o [a, b]
  | o == '+' = Right $ a + b
  | o == '-' = Right $ a - b
  | o == '*' = Right $ a * b
  | o == '/' = Right $ a / b
  | o == '^' = Right $ a ** b
  | o == '%' = errorMessege (intFuncDouble mod a b) "mod error"
  | o == 'd' =
      let x = do
            num <- toIntegral a
            sides <- toIntegral b
            Dice.rollDice gen num sides
       in errorMessege (fromIntegral <$> x) "dice error"
  | otherwise = Left "unknown operator"
applyOperator gen o [a]
  | o == '+' = Right a
  | o == '-' = Right $ -a
  | o == '~' = Right $ -a
  | o == '!' = errorMessege (intFuncSingle' fac a) "factorial error"

applyFunction :: StdGen -> String -> [Double] -> ErrorProne Double
applyFunction gen o []
  | o == "pi" = Right pi
  | o == "e" = Right $ exp 1
  | otherwise = Left "unknown constant"
applyFunction gen o [x]
  | o == "negate" = Right $ negate x
  | o == "abs" = Right $ abs x
  | o == "sign" = Right $ signum x
  | o == "round" = Right $ fromIntegral $ round x
  | o == "trunc" = Right $ fromIntegral $ truncate x
  | o == "floor" = Right $ fromIntegral $ floor x
  | o == "ceil" = Right $ fromIntegral $ ceiling x
  | o == "exp" = Right $ exp x
  | o == "sqrt" = Right $ sqrt x
  | o == "ln" = Right $ log x
  | o == "log" = Right $ logBase 10 x
  | o == "fac" = errorMessege (intFuncSingle' fac x) "factorial error"
  | o == "sin" = Right $ sin x
  | o == "tan" = Right $ tan x
  | o == "cos" = Right $ cos x
  | o == "asin" = Right $ asin x
  | o == "atan" = Right $ atan x
  | o == "acos" = Right $ acos x
  | o == "rad" = Right $ x / 180 * pi
  | o == "deg" = Right $ x / pi * 180
applyFunction gen o [a, b]
  | o == "add" = Right $ a + b
  | o == "sub" = Right $ a - b
  | o == "mult" = Right $ a * b
  | o == "div" = Right $ a / b
  | o == "mod" = errorMessege (intFuncDouble mod a b) "mod error"
  | o == "pow" = Right $ a ** b
  | o == "log" = Right $ logBase b a
applyFunction gen o l@(x : xs)
  | o == "sum" = Right $ sum l
  | o == "prod" = Right $ product l
  | o == "min" = Right $ minimum l
  | o == "max" = Right $ maximum l
  | otherwise = Left "unknown function"

evaluate :: StdGen -> ErrorProne (Tree Token) -> ErrorProne Double
evaluate _ (Left e) = Left e
evaluate gen (Right (Leaf x)) = case x of
  Number n -> Right n
  _ -> Left "bad input"
evaluate gen (Right (Branch x l)) = case x of
  Operator c -> mapM (evaluate gen . Right) l >>= applyOperator gen c
  Function s -> mapM (evaluate gen . Right) l >>= applyFunction gen s
  _ -> Left "bad input"