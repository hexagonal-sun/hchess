module Main where

import Locus
import PrettyPrint
import MoveGen
import Game

main :: IO ()
main = do
  let g = newGame
  pp g
