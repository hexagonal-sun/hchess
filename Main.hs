module Main where

import PrettyPrint
import MoveGen
import Game

perft :: Int -> GameState -> Int
perft 0 _  = 1
perft n state = sum $ map (perft (n - 1)) $ moveGen state

main :: IO ()
main = do
  let g = newGame
  pp g
  putStrLn $ "pert 0: " ++ show (perft 2 g)
