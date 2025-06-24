module Operators (ternaryOperators, infixOperators, prefixOperators, postfixOperators, operatorCharacters) where

import Data.List (union)

thd :: (a, b, c) -> c
thd (_, _, c) = c

-- character, precedence, left associative, secondary characters

ternaryOperators :: [(Char, (Int, Bool, [Char]))]
ternaryOperators =
  [ ('d', (5, True, ['t', 'b']))
  ]

infixOperators :: [(Char, (Int, Bool))]
infixOperators =
  [ ('+', (1, True)),
    ('-', (1, True)),
    ('*', (2, True)),
    ('/', (2, True)),
    ('%', (2, True)),
    ('^', (4, False)),
    ('d', (5, True))
  ]

prefixOperators :: [(Char, Int)]
prefixOperators =
  [ ('+', 3),
    ('-', 3),
    ('~', 3),
    ('d', 5)
  ]

postfixOperators :: [(Char, Int)]
postfixOperators =
  [('!', 4)
  ]

operatorCharacters = union (map fst ternaryOperators) $ union (foldl1 union (map (thd . snd) ternaryOperators)) $ union (map fst infixOperators) $ union (map fst prefixOperators) (map fst postfixOperators)