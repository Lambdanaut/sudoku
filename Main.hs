module Main where

import Control.Monad
import Data.Vector as V
import Safe

type Puzzle = Vector (Maybe Int)
type Row    = Vector (Maybe Int)
type Column = Vector (Maybe Int)

puzzle_file = "puzzle"



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
	return ()
