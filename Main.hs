module Main where

import Control.Monad
import Data.Maybe
import Data.Vector as V
import Safe

type Puzzle = Vector (Maybe Int)
type Row    = Vector (Maybe Int)
type Column = Vector (Maybe Int)

puzzle_file = "puzzle"

-- Fewest unfilled spots is better
-- This is a beautiful function
heuristic :: Puzzle -> Int
heuristic puzzle = V.length $ V.filter isNothing puzzle

-- Pseudocode for problem solver
-- 
-- 1) Add the current state to the frontier (Nothing)
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

get_row :: Int -> Puzzle -> Row
get_row row_index puzzle = slice (row_index * 9) (row_index * 9 + 9) puzzle

get_column :: Int -> Puzzle -> Column
get_column column_index puzzle = generate 9 (\x -> puzzle ! ((x * 9) + column_index) )

load_puzzle :: FilePath -> IO Puzzle
load_puzzle f = do
	read_puzzle <- readFile f
	let stripped_puzzle = Prelude.filter (\char -> char /= '\n') read_puzzle
	return $ parse_puzzle stripped_puzzle

parse_puzzle :: String -> Puzzle
parse_puzzle unparsed = fromList $ fmap (\x-> readMay [x]) unparsed

main :: IO ()
main = do
	puzzle <- load_puzzle puzzle_file
	print puzzle
