# Operators and Functions

## Operators

Supported operators are `~`, `+`, `-`, `*`, `/`, `%`, `^`, and `!`

- `~` is a dedicated negation operator.
- `+` and `-` are addition and subtration, respectively. As prefixes, they are the identity function and negation.
- `*` and `/` are multiplication and division. Division by zero will result in Infinity.
- `%` is modulo. This will throw an error if either input is non-intergral. In addition, the dividend mush be non-zero.
- `^` is exponentiation.
- `!` is factorial. This will throw an error if the input is non-integral.

## Functions

Multiple functions are supported. All trigonametric functions operate using radians.  
Functions take the form *function*(*argument 1*, *argument 2*, …, *argument n*)

Single Argument Functions:
- `negate` — Negation
- `abs` — Absolute value
- `sign` — The sign of *n*. -1 if *n* is neagtive, 0 if *n* is 0, and 1 if *n* is positive. abs x * sign x == x
- `round` — Rounds *n* to the nearest integer
- `trunc` — Truncates everything right of the decimal point. Equivalent to floor when *n* > 0 and ceil when *n* < 0
- `floor` — Round towards negtive infinity
- `ceil` — Round towards positive infinity
- `inv` — Reciprocal
- `exp` — equivalent to e^*n*
- `sqrt` — Square root. Returns NaN for negative inputs
- `ln` — Natural log. Returns NaN for negative inputs and -Infinity for 0
- `log` — Log base 10. Returns NaN for negative inputs and -Infinity for 0
- `fac` — Factorial. Throws an error for non-integral inputs
- `sin` — Sine
- `tan` — Tangent
- `cos` — Cosine
- `asin` — Inverse sine
- `atan` — Inverse Tangent
- `acos` — Inverse Cosine
- `rad` — Convert degrees to radians
- `deg` — Convert radians to degrees

Two Argument Functions:
- `add` — Addition
- `sub` — Subtraction
- `mult` — Multiplication
- `div` — Division
- `mod` — Modulo. Throws an error if either input is non-intergral or the dividend is zero
- `rem` — Remainder. Throws an error if either input is non-intergral or the dividend is zero
- `pow` — Exponentiation
- `log` — log base *b* or *a*

Multiple Argument Functions: (takes any number of argments)
- `sum` — The sum of every argument
- `prod` — The product of every argument multiplied together
- `min` — The smallest argument
- `max` — The largest argument

## Constants

Supported constants are pi, tau (2π), and e (euler's number)