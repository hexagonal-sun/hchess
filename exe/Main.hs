module Main where

import Uci
import Game
import Control.Monad.State

main :: IO ()
main = evalStateT uciMain newGame
