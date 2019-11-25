# Sudoku Solver

![[standard-readme compliant](https://img.shields.io/badge/readme%20style-standard-brightgreen.svg?style=flat-square)](https://github.com/RichardLitt/standard-readme)

>A basic sudoku solver written in Haskell.

This is a fairly simple sudoku solver written in Haskell. Each function has extensive inline documentation to help understand how everything works together.

## Install

This project only requires the Haskell runtime to use. If you don't already have Haskell, it is recommended to use the [Haskell Platform](https://www.haskell.org/platform/).

## Usage
### Program Setup
Clone a local copy of [sudoku.hs](sudoku.hs) and navigate to the directory you saved it to in your terminal, then run the following command:

```sh
ghc sudoku.hs
```

### Puzzle Setup

Format your unsolved sudoku puzzle in a text file as shown in [example.txt](example.txt), using zeros for blank spaces.

### Running
Compile the haskell script and run the executable with the puzzle text file you created as the only command line argument.
