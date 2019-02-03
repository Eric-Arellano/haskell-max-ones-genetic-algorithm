# Haskell max ones genetic algorithm
CSE 598 project to find the bit string with the most ones through a genetic algorithm.

Ended up finishing project with Python due to frustrations having to continually pass a new RandomGen every time I needed some random value. This became confusing and made the code more complicated. See https://github.com/Eric-Arellano/python-max-ones-genetic-algorithm.

## Prerequisites
1. [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

## To install
Assumes Haskell Stack installed.

1. `stack setup`

## To run
1. `stack build`
1. `stack exec max-ones-exe`

### REPL
1. `stack ghci`
1. `main`

##### Update project
`:r`

##### Quit REPL
`:q`

## To run tests
1. `stack test`

### REPL
1. `stack ghci max-ones-ga:max-ones-test`
1. `main`

##### Update project
`:r`

##### Quit REPL
`:q`

## Things I learned
* The importance of a random seed for reproducibility. I never thought about this parameter until Haskell's pure functions made me.
* How to install 3rd party libraries.
* How to add and use language extensions, namely `ScopedTypeVariables`.
* QuickCheck
