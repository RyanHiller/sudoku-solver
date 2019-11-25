-- Author(s): Ryan Hiller
-- Notes: -I'm still fairly new to Haskell so I know there are more efficient ways to
--         implement areBoxesValid and buildChoices, but they still work.
--        -I used a couple of utility functions that I found that made the todo work a little easier
--         in some cases. They are right above main.

import System.Environment
import System.IO
import Data.Containers.ListUtils
import Data.List
import Control.Monad

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- name: toInt
-- description: converts given parameter to integer
-- input: a string
-- output: the string converted to integer
-- example: toInt "123" returns 123
toInt :: [Char] -> Int
toInt s = read s :: Int

-- name: toIntList
-- description: converts given parameter to a sequence of integers (one digit at a time)
-- input: a string
-- output: the string converted into a sequence of integers
-- example: toIntList "123" returns [1, 2, 3]
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

-- ***** GETTER FUNCTIONS *****

-- name: getBoard
-- description: convert given string to a sudoku board
-- input: a string (the board as read from a sudoku input file)
-- output: a sudoku board
-- example: getBoard "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n" yields
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
getBoard :: [Char] -> Board
getBoard s = map toIntList(lines s)

-- name: getNRows
-- description: given a board, return its number of rows
-- input: a board
-- output: number of rows
-- example: getNRows
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields 9
getNRows :: Board -> Int
getNRows s = length s

-- name: getNCols
-- description: given a board, return its number of columns or 0 if rows do not have the same number of columns
-- input: a board
-- output: number of columns
-- example 1: getNCols
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields 9
-- example 2: getNCols
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,6,0],
--   [8,0,0,0,6,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields 0
getNCols :: Board -> Int
getNCols s =
  let lens = map length s
  in if ((all (== head lens)) (tail lens)) == True
    then head lens
  else 0

-- name: getBox
-- description: given a board and box coordinates, return the correspondent box as a sequence
-- input: a board and two integer (box coordinates)
-- output: a sequence
-- example: getBox
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 1 1 yields [0,8,0,6,0,2,0,3,0]
getBox :: Board -> Int -> Int -> Sequence
getBox b y x =
  let rows = slice (y * 3) ((y * 3) + 2) b
      box = map (slice (x * 3) ((x * 3) + 2)) rows
  in join (transpose box)

-- name: getEmptySpot
-- description: given a board, return the first location that is empty (i.e., it has zero), if one exists; OK to assume that you will only call this function when you know that there is an empty spot
-- input: a board
-- output: a tuple with the coordinates (i, j) of the empty spot found
-- example: getEmptySpot
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields (0,2)
getEmptySpot :: Board -> (Int, Int)
getEmptySpot b = 
  let indices = map elim (map (findIndex (==0)) b)
      row = (elim (findIndex (/= -1) indices))
      spot = (row, (indices !! row))
  in if row == -1
    then (-1, -1)
  else spot

-- ***** PREDICATE FUNCTIONS *****

-- name: isGridValid
-- description: given a board, return True/False depending whether the given board constitutes a valid grid (i.e., #rows = #cols = 9) or not
-- input: a board
-- output: True/False
-- example 1: isGridValid
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields True
-- example 2:
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns False
-- example 3:
-- [ [5,3,0,7,0,0,0,0],
--   [6,0,1,9,5,0,0,0],
--   [8,0,0,6,0,0,0,3],
--   [4,0,8,0,3,0,0,1],
--   [7,0,0,2,0,0,0,6],
--   [0,0,0,0,0,2,8,0],
--   [0,0,4,1,9,0,0,5],
--   [0,0,0,8,0,0,7,9] ] returns False
isGridValid :: Board -> Bool
isGridValid b = ((getNRows b == 9) && (getNCols b == 9))

-- name: isSequenceValid
-- description: return True/False depending whether the given sequence is valid or not, according to sudoku rules
-- input: a sequence of digits from 0-9
-- output: True/False
-- example 1: isSequenceValid [5,3,0,0,7,0,0,0,0] yields True
-- example 2: isSequenceValid [5,3,0,5,7,0,0,0,0] yields False
isSequenceValid :: Sequence -> Bool
isSequenceValid s =
  let lst = filter (/= 0) s
      lst2 = nubInt lst
  in (length lst) == (length lst2)

-- name: areRowsValid
-- description: return True/False depending whether ALL of the row sequences are valid or not
-- input: a board
-- output: True/False
areRowsValid :: Board -> Bool
areRowsValid b = all (==True) (map isSequenceValid b)

-- name: areColsValid
-- description: return True/False depending whether ALL of the col sequences are valid or not
-- input: a board
-- output: True/False
areColsValid :: Board -> Bool
areColsValid b =
  areRowsValid (transpose b)

-- name: areBoxesValid
-- description: return True/False depending whether ALL of the box sequences are valid or not
-- input: a board
-- output: True/False
areBoxesValid :: Board -> Bool
areBoxesValid b = 
  let boxes = (isSequenceValid (getBox b 0 0)):(isSequenceValid (getBox b 0 1)):(isSequenceValid (getBox b 0 2)):(isSequenceValid (getBox b 1 0)):(isSequenceValid (getBox b 1 1)):(isSequenceValid (getBox b 1 2)):(isSequenceValid (getBox b 2 0)):(isSequenceValid (getBox b 2 1)):(isSequenceValid (getBox b 2 2)):[]
  in all (==True) boxes

-- name: isValid
-- description: return True/False depending whether the given board is valid sudoku configuration or not
-- input: a board
-- output: True/False
isValid :: Board -> Bool
isValid b = (areRowsValid b) && (areColsValid b) && (areBoxesValid b)

-- name: isCompleted
-- description: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
-- input: a board
-- output: True/False
isCompleted :: Board -> Bool
isCompleted b =
  if getEmptySpot b == (-1, -1)
    then True
  else False

-- name: isSolved
-- description: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
-- input: a board
-- output: True/False
isSolved :: Board -> Bool
isSolved b = (isCompleted b) && (isValid b)

-- ***** SETTER FUNCTIONS *****

-- name: setRowAt
-- description: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
-- input: a sequence, an index, and a value
-- output: a new sequence
-- example 1: setRowAt [1, 2, 3, 0, 4, 5] 3 9 yields [1,2,3,9,4,5]
-- example 2: setRowAt [1, 2, 3, 8, 4, 5] 3 9 yields [1,2,3,8,4,5]
setRowAt :: Sequence -> Int -> Int -> Sequence
setRowAt seq idx val =
  if seq !! idx == 0
    then (take idx seq) ++ val:[] ++ (drop (idx + 1) seq)
  else seq

-- name: setBoardAt
-- description: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
-- input: a board, two indexes (i, j), and a value
-- output: a new board
-- example 1: setBoardAt
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 4 yields
-- [ [5,3,4,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
setBoardAt :: Board -> Int -> Int -> Int -> Board
setBoardAt b row col val =
  let seq = setRowAt (b !! row) col val
  in if seq /= (b !! row)
    then (take row b) ++ seq:[] ++ (drop (row + 1) b)
  else b

-- name: buildChoices
-- description: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; assumes that the cell at (i, j) is empty
-- input: a board and two indexes (i, j)
-- output: a list of boards from the original board
-- example: buildChoices
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 yields
-- [
-- [ [5,3,1,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- [ [5,3,2,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- ...
-- [ [5,3,9,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- ]
buildChoices :: Board -> Int -> Int -> [Board]
buildChoices b row col =
  --(setBoardAt b row col 1) ++ (setBoardAt b row col 2) ++ (setBoardAt b row col 3) ++ (setBoardAt b row col 4) ++ (setBoardAt b row col 5) ++ (setBoardAt b row col 6) ++ (setBoardAt b row col 7) ++ (setBoardAt b row col 8) ++ (setBoardAt b row col 9)
  let b2 = setBoardAt b row col 1:[] ++ setBoardAt b row col 2:[] ++ setBoardAt b row col 3:[] ++ setBoardAt b row col 4:[] ++ setBoardAt b row col 5:[] ++ setBoardAt b row col 6:[] ++ setBoardAt b row col 7:[] ++ setBoardAt b row col 8:[] ++ setBoardAt b row col 9:[]
  in b2

-- name: solve
-- description: given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
-- input: a board
-- output: a list of boards from the original board
solve :: Board -> [Board]
solve board
  | isSolved board = [board]
  | isCompleted board = [[[]]]
  | not (isValid board) = [[[]]]
  | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
    where
      emptySpot = getEmptySpot board
      i = fst emptySpot
      j = snd emptySpot

-- ***** UTILITY FUNCTIONS *****

-- name: slice
-- description: given a list and two indices, returns a new list of values at the index range provided (inclusive)
-- input: a list
-- output: a new sublist created from input
-- example: slice 1 3 [a, b, c, d, e, f] yields [b, c, d]
-- note: i got this code from https://stackoverflow.com/a/4597898
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- name: elim
-- description: given a Maybe Int, returns the corresponding Int or -1 if Nothing
-- input: Maybe Int
-- output: matching Int of input or -1
-- example elim [Just 1, Just 3, Just 6, Nothing, Just 8] yields [1, 3, 6, -1, 8]
-- note: this is here to handle the output of the findIndex function
elim :: Maybe Int -> Int
elim a =
  case a of (Just a) -> a
            Nothing -> -1

-- program starts here
main = do
  -- validate the command-line and get the file name containing the board
  args <- getArgs

  -- read the contents of the board file into a string
  str <- readFile (args !! 0)

  -- create a board from the string board (hint: use getBoard)
  let board = getBoard str

  -- use solve to find the solutions, disconsidering the ones that are [[]]
  let solved = solve board

  -- TODO #21: print the solutions found
  -- Issue with the solve function
  print (filter (/= [[]]) solved)

  print "Done!"
