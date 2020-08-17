module Main where

import Locus
import Board
import PrettyPrint
import MoveGen

main :: IO ()
main = do
  let board = startingBoard
  pp board
  putStrLn "Bishop"
  let boards = pseudoMoveGen board (FC, R1)
  mapM_ pp boards
  putStrLn "Pawn"
  let boards = pseudoMoveGen board (FB, R2)
  mapM_ pp boards