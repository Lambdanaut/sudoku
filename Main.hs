module Main where

import Control.Monad
import Data.List
import Data.List.Split (chunksOf)
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
solve puzzle = _solve S.empty (S.fromList [(puzzle, heuristic puzzle)])

_solve :: S.Set Puzzle -> S.Set (Puzzle, Int) -> Puzzle
_solve explored frontier = if done_check then lowest_h_puzzle else _solve explored_2 frontier_3
  where
	-- TODO: Fail if the frontier is empty

	-- Get the best scoring heuristic on the frontier. This is our new current puzzle.
	--frontier_1 = S.map (\puzzle -> (puzzle, heuristic puzzle) ) frontier
	lowest_h = Data.Foldable.minimumBy (\(_, h1) (_, h2) -> compare h1 h2) frontier
	lowest_h_puzzle = fst lowest_h

	-- Check if we're done yet
	done_check = solved lowest_h_puzzle || S.null frontier

	-- Switch the current puzzle from the frontier to the explored list
	frontier_2 = S.delete lowest_h frontier
	explored_2 = S.insert lowest_h_puzzle explored

	-- Build a list of all possible legal board moves and the states they move to
	magic_grid =  [(row, column) | row <- [0..8], column <- [0..8] ]
	possible_moves = S.fromList $ concat $ map (\(row, column) -> map ((set_index lowest_h_puzzle row column).Just) $ legal_moves lowest_h_puzzle row column) magic_grid
	possible_moves_heuristics = S.map (\move -> (move, heuristic move)) possible_moves

	-- Ignore the move if it has already been explored
	unexplored_possible_moves = S.filter (\(move,_) -> S.notMember move explored_2) possible_moves_heuristics

	-- Add the newfound moves to the frontier
	frontier_3 = S.union frontier_2 unexplored_possible_moves


solved :: Puzzle -> Bool
solved puzzle = V.all isJust puzzle 

-- Fewest unfilled squares is better
heuristic :: Puzzle -> Int
heuristic puzzle = V.length $ V.filter isNothing puzzle

heuristic_2 :: Puzzle -> Int -> Int -> Int
heuristic_2 old_puzzle row column = length $ legal_moves old_puzzle row column

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

show_puzzle :: Puzzle -> String
show_puzzle puzzle = concat.map ('\n':) $ chunksOf 9 single_line_puzzle
	where single_line_puzzle = V.toList $ V.map (\square -> if unJust square == 0 then ' ' else head $ show $ unJust square ) puzzle

main :: IO ()
main = do
	puzzle <- load_puzzle_
	putStrLn $ show_puzzle $ solve puzzle
