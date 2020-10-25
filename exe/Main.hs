module Main where

import Uci
import Fen
import Control.Monad.State

main :: IO ()
main = evalStateT uciMain newGame
