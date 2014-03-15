module Main where

import Control.Monad
import Control.Monad.State.Lazy
import Data.Maybe
import Data.Vector as V
import Safe

-- The Sudoku Puzzle is represented as a 1-Dimensional array containing Maybes.
-- A "Nothing" value is an unfilled square.
-- A "Just x" value is a filled in square of value "x".
type Puzzle = Vector Square
type Row    = Vector Square
type Column = Vector Square
type Square = Maybe Int

puzzle_file = "puzzle"

-- Fewest unfilled squares is better
-- This is a beautiful function
heuristic :: Puzzle -> Int
heuristic puzzle = V.length $ V.filter isNothing puzzle

-- Pseudocode for problem solver
-- 
-- 1) Add the current state to the frontier  *
-- 2) Repeat the following
--     a) Look for the lowest heuristic cost on the frontier. Set it as the current state.
--     b) Switch it to the explored list.
--     c) For every space on the board: 
--         * Ignore it if it is filled in, illegal to fill in, or on the explored list.
--	       * If it isn’t on the frontier, add it to the frontier. 
--           Make the current state the parent of this state. (Add in a backtrack function) 
--           Record the heuristic cost of this state
--     d) Stop when: 
--         * there are no empty spaces on the puzzle
--         * the frontier is empty, in which case the puzzle is unsolvable
--  3) Return the final state

data SolveState = SolveState {
	  ss_puzzle   :: Puzzle
	, ss_explored :: Vector Puzzle
	, ss_frontier :: Vector Puzzle
} deriving Show

-- Gets the value from running the stateful _solve function
solve :: Puzzle -> Puzzle
solve starting_puzzle = fst $ runState _solve solve_state
        -- The beginning state for the problem. Adds the current puzzle to the frontier. 
  where solve_state = SolveState starting_puzzle V.empty (V.singleton starting_puzzle)

_solve :: State SolveState Puzzle
_solve = do

	return $ fromList []
  		


solved :: Puzzle -> Bool
solved puzzle = V.all isJust puzzle 

legal_move :: Puzzle -> Int -> Int -> Int -> Bool
legal_move puzzle move_value row column = empty_square && not value_conflicts
  where
	square_value    = get_index puzzle row column
	square_row      = get_row puzzle row
	square_column   = get_column puzzle column
	empty_square    = isNothing square_value
	value_conflicts = V.elem (Just move_value) (square_row V.++ square_column)

get_index :: Puzzle -> Int -> Int -> Square
get_index puzzle row column = puzzle ! (row * 9 + column)

get_row :: Puzzle -> Int -> Row
get_row puzzle row_index = slice (row_index * 9) (row_index * 9 + 9) puzzle

get_column :: Puzzle -> Int -> Column
get_column puzzle column_index = generate 9 (\x -> puzzle ! ((x * 9) + column_index) )

load_puzzle :: FilePath -> IO Puzzle
load_puzzle f = do
	read_puzzle <- readFile f
	let stripped_puzzle = Prelude.filter (\char -> char /= '\n') read_puzzle
	return $ parse_puzzle stripped_puzzle

load_puzzle_ :: IO Puzzle
load_puzzle_ = load_puzzle puzzle_file

parse_puzzle :: String -> Puzzle
parse_puzzle unparsed = fromList $ fmap (\x-> readMay [x]) unparsed

main :: IO ()
main = do
	puzzle <- load_puzzle_
	print puzzle
