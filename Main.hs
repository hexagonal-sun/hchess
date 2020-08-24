module Main where

import MoveGen
import Game

perft :: Int -> GameState -> Int
perft 0 _  = 1
perft n state = sum $ map (\(_,_,s) -> perft (n - 1) s) $ moveGen state

main :: IO ()
main = do
  let g = newGame
  mapM_ (\n -> putStrLn $ "perft " ++ show n ++ ": " ++ show (perft n g)) [1..4]
