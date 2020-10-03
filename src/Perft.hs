module Perft (
  perftInt,
  perft,
  perftSplit) where

import Game
import PrettyPrint
import MoveGen
import Fen
import Locus
import Move
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Void
import System.IO

type Parser = Parsec Void String

pRank :: Parser Rank
pRank = choice
  [ R1 <$ char '1'
  , R2 <$ char '2'
  , R3 <$ char '3'
  , R4 <$ char '4'
  , R5 <$ char '5'
  , R6 <$ char '6'
  , R7 <$ char '7'
  , R8 <$ char '8']

pFile :: Parser File
pFile = choice
  [ FA <$ char 'a'
  , FB <$ char 'b'
  , FC <$ char 'c'
  , FD <$ char 'd'
  , FE <$ char 'e'
  , FF <$ char 'f'
  , FG <$ char 'g'
  , FH <$ char 'h']

pLocus :: Parser Locus
pLocus = do
  file <- pFile
  rank <- pRank
  return $ frToLoc (file,rank)

pMove :: Parser Move
pMove = do
  src <- pLocus
  dst   <- pLocus
  return $ Move src dst Nothing

perftInt :: Int -> GameState -> Int
perftInt 0 _  = 1
perftInt n state = sum $ map (perftInt (n - 1)) $ moveGen state

perft :: GameState -> Int -> IO ()
perft state n = do
  let perfts = map (\s -> (s, perftInt (n - 1) s)) $ moveGen state
  mapM_ (\(g, num) -> (pp $ head $ madeMoves g) >> putStr ": " >> print num) perfts
  let total = foldl (\x (_,p) -> x + p) 0 perfts
  putStrLn $ "Perft " ++ show n ++ " total: " ++ show total

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

readMove :: GameState -> IO GameState
readMove game = do
  moveStr <- prompt "Next move: "
  case parse pMove "<input>" moveStr of
    Left bundle -> do
      putStr "Could not parse move: "
      putStrLn $ errorBundlePretty bundle
      readMove game
    Right m -> case makeMove game m of
      Nothing -> do
        putStrLn $ "Error: " ++ moveStr ++ " is invalid."
        readMove game
      Just nextGame ->
        return nextGame

perftSplit' :: GameState -> Int -> IO ()
perftSplit' game 1 = pp game >> perft game 1
perftSplit' game n = do
  pp game
  perft game n
  nextState <- readMove game
  perftSplit' nextState $ n - 1

perftSplit :: IO ()
perftSplit = do
  ln <- prompt "Position (FEN): "
  case parseFen ln of
    Left e -> print e >> perftSplit
    Right game -> do
      n <- prompt "Number of perft splits: "
      perftSplit' game $ read n
