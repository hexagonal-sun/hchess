module Main where

import MoveGen
import Game

perft' :: Int -> GameState -> Int
perft' 0 _  = 1
perft' n state = sum $ map (\(_,_,s) -> perft' (n - 1) s) $ moveGen state

perft :: GameState -> Int -> IO ()
perft state n = do
  let states = moveGen state
  let perfts = map (\(f, t, s) -> (f,t, perft' (n - 1) s)) states
  mapM_ print perfts
  let total = foldl (\x (_,_,p) -> x + p) 0 perfts
  putStrLn $ "Perft " ++ show n ++ " total: " ++ show total

main :: IO ()
main = do
  let g = newGame
  mapM_ (perft g) [1..4]
