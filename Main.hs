module Main where

import Board
import PrettyPrint

main :: IO ()
main = do
  let board = startingBoard
  pp board
