# HaskellDiceCalculator

A calculator that allows dice notation, written in haskell

## Usage

Dice rolls are in the standard format:  
[number of dice]d[number of sides on each die]

in addition, the highest or lowest dice can be removed by adding 't' (top) or 'b' (bottom) to the end, respectively, along with the amount to be removed:  
#d#t[remove highest n values]  
#d#b[remove lowest n values]

Basic math operators supported, along with a number of functions. More information [here](./OperatorsAndFunctions.md)

Some common operations have shortcuts  
1d20: T  
Advantage (2d20b1): A  
Disadvantage (2d20t1): D  
Stats Rolling (4d6b1): S  

Everything is case insensitive

Type 'exit' to quit
