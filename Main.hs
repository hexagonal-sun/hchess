module Main where

import Board
import PrettyPrint
import MoveGen
import Game
import Data.Ix

perft :: Int -> GameState -> Int
perft 0 _  = 1
perft n state = sum $ map (perft (n - 1)) subStates
  where subStates = concatMap (moveGen state) $ range boardBounds

main :: IO ()
main = do
  let g = newGame
  pp g
  putStrLn $ "pert 0: " ++ (show $ perft 3 g)
