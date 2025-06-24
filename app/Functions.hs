module Functions (constants, oneArgFunctions, twoArgFunctions, multiArgFunctions) where

constants :: [[Char]]
constants = ["pi", "tau", "e"]

oneArgFunctions :: [[Char]]
oneArgFunctions = ["negate", "abs", "sign", "round", "trunc", "floor", "ceil", "inv", "exp", "sqrt", "ln", "log", "fac", "sin", "tan", "cos", "asin", "atan", "acos", "rad", "deg"]

twoArgFunctions :: [[Char]]
twoArgFunctions = ["add", "sub", "mult", "div", "mod", "rem", "pow", "log"]

multiArgFunctions :: [[Char]]
multiArgFunctions = ["add", "mult", "sum", "prod", "min", "max"]
