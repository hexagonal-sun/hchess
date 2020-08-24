module Main where

import MoveGen
import Game

perft :: Int -> GameState -> Int
perft 0 _  = 1
perft n state = sum $ map (perft (n - 1)) $ moveGen state

main :: IO ()
main = do
  let g = newGame
  mapM_ (\n -> putStrLn $ "perft " ++ show n ++ ": " ++ show (perft n g)) [1..4]
