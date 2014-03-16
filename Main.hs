module Main where

import Control.Monad
import Data.List
import Data.Foldable (minimumBy)
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Vector as V
import Safe (readMay)

-- The Sudoku Puzzle is represented as a 1-Dimensional array containing Maybes.
-- A "Nothing" value is an unfilled square.
-- A "Just x" value is a filled in square of value "x".
type Puzzle = V.Vector Square
type Row    = V.Vector Square
type Column = V.Vector Square
type Square = Maybe Int

puzzle_file = "puzzle"

-- Pseudocode for problem solver
-- 
-- 1) Add the current state to the frontier
-- 2) Repeat the following
--     a) Look for the lowest heuristic cost on the frontier. Set it as the current state.
--     b) Switch it to the explored list.
--     c) For every legal, non-filled in possible move on the board: 
--         * Ignore it if the move leads to a state on the explored list
--	       * If it isn't on the frontier, add it to the frontier. 
--           Make the current state the parent of this state. (Add in a backtrack function) 
--           Record the heuristic cost of this state
--     d) Stop when: 
--         * there are no empty spaces on the puzzle
--         * the frontier is empty, in which case the puzzle is unsolvable
--  3) Return the final state

-- Takes an unsolved puzzle. Returns a solved puzzle
solve :: Puzzle -> Puzzle
-- The beginning state for the problem. Adds the current puzzle to the frontier. 
solve puzzle = _solve puzzle S.empty (S.fromList [puzzle])

_solve :: Puzzle -> S.Set Puzzle -> S.Set Puzzle -> Puzzle
_solve puzzle explored frontier = V.fromList []
  where
	-- TODO: Fail if the frontier is empty

	-- Get the best scoring heuristic on the frontier. This is our new current state.
	frontier_h = S.map (\puzzle -> (puzzle, heuristic puzzle) ) frontier
	lowest_h = fst $ Data.Foldable.minimumBy (\(_, h1) (_, h2) -> compare h1 h2) frontier_h

	-- Switch the current puzzle from the frontier to the explored list
	new_frontier = S.delete lowest_h frontier
	new_explored = S.insert lowest_h explored

	-- Build a list of all possible legal board moves and the states they move to
	magic_grid =  [(row, column) | row <- [0..8], column <- [0..8] ]
	possible_moves = map (\(row, column) -> map ((set_index lowest_h row column).Just) $ legal_moves lowest_h row column) magic_grid


	-- Add future state data to all possible moves


	-- Filter out all moves that lead to states that have already been explored

	-- Continue to the next state
	--next_state = _solve lowest_h NEW_EXPLORED_HERE NEW_FRONTIER_HERE


solved :: Puzzle -> Bool
solved puzzle = V.all isJust puzzle 

-- Fewest unfilled squares is better
-- This is a beautiful function
heuristic :: Puzzle -> Int
heuristic puzzle = V.length $ V.filter isNothing puzzle

legal_move :: Puzzle -> Int -> Int -> Int -> Bool
legal_move puzzle move_value row column = elem move_value $ legal_moves puzzle row column

-- Returns a list of legal moves for a given square
legal_moves :: Puzzle -> Int -> Int -> [Int]
legal_moves puzzle row column
	| isJust square_value = []
	| otherwise = filter (\square -> notElem square square_neighbors) [1..9]
  where
	square_value     = get_index puzzle row column
	square_row       = get_row puzzle row
	square_column    = get_column puzzle column
	square_neighbors = map unJust $ filter isJust $ V.toList $ square_row V.++ square_column

get_index :: Puzzle -> Int -> Int -> Square
get_index puzzle row column = puzzle V.! (row * 9 + column)

set_index :: Puzzle -> Int -> Int -> Square -> Puzzle
set_index puzzle row column square_value = puzzle V.// [(row * 9 + column, square_value)]

get_row :: Puzzle -> Int -> Row
get_row puzzle row_index = V.slice (row_index * 9) 9 puzzle

get_column :: Puzzle -> Int -> Column
get_column puzzle column_index = V.generate 9 (\x -> puzzle V.! ((x * 9) + column_index) )

load_puzzle :: FilePath -> IO Puzzle
load_puzzle f = do
	read_puzzle <- readFile f
	let stripped_puzzle = filter (\char -> char /= '\n') read_puzzle
	return $ parse_puzzle stripped_puzzle

load_puzzle_ :: IO Puzzle
load_puzzle_ = load_puzzle puzzle_file

parse_puzzle :: String -> Puzzle
parse_puzzle unparsed = V.fromList $ fmap (\x-> readMay [x]) unparsed

-- Util function
unJust :: Square -> Int
unJust (Just x) = x
unJust Nothing  = 0

main :: IO ()
main = do
	puzzle <- load_puzzle_
	print puzzle
