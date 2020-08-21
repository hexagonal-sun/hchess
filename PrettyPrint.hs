{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint (
  PrettyPrint,
  pp
) where
import Data.Char
import Data.Array
import Piece
import Board
import Locus
import Game

class PrettyPrint a where
  pp :: a -> IO ()

getPieceKindChar :: PieceKind -> Char
getPieceKindChar Pawn   = 'p'
getPieceKindChar Rook   = 'r'
getPieceKindChar Bishop = 'b'
getPieceKindChar Knight = 'n'
getPieceKindChar King   = 'k'
getPieceKindChar Queen  = 'q'

instance PrettyPrint Rank where
  pp x = putChar $ case x of
    R8 -> '8'
    R7 -> '7'
    R6 -> '6'
    R5 -> '5'
    R4 -> '4'
    R3 -> '3'
    R2 -> '2'
    R1 -> '1'

instance PrettyPrint File where
  pp x = putChar $ case x of
    FA-> 'A'
    FB-> 'B'
    FC-> 'C'
    FD-> 'D'
    FE-> 'E'
    FF-> 'F'
    FG-> 'G'
    FH-> 'H'
 
instance PrettyPrint Piece where
  pp (Piece White k) = putChar $ toUpper $ getPieceKindChar k
  pp (Piece Black k) = putChar $ toLower $ getPieceKindChar k

instance PrettyPrint SquareState where
  pp Nothing = putChar '.' >> putChar ' '
  pp (Just p) = pp p >> putChar ' '

putRank :: BoardState -> Rank -> IO ()
putRank board rank = do
  pp rank >> putChar ' '
  let idxes = [(f, rank) | f <- [minBound..] ::[File]]
  mapM_ (pp . (board !)) idxes

instance PrettyPrint BoardState where
  pp board = do
    mapM_ (\r -> putRank board r >> putStrLn "") (reverse [minBound..] :: [Rank])
    putStr "  "
    mapM_ (\f -> pp f >> putChar ' ') ([minBound..] :: [File])
    putStrLn ""

instance PrettyPrint GameState where
  pp (GameState board nextColour) = do
    pp board
    putStr "Next to move: "
    print nextColour
