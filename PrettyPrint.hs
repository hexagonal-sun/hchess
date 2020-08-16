{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint (
  PrettyPrint,
  pp
) where
import Data.Char
import Data.List
import Data.Array
import Piece
import Board
import Locus

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
  pp R8 = putChar '8'
  pp R7 = putChar '7'
  pp R6 = putChar '6'
  pp R5 = putChar '5'
  pp R4 = putChar '4'
  pp R3 = putChar '3'
  pp R2 = putChar '2'
  pp R1 = putChar '1'

instance PrettyPrint File where
  pp FA = putChar 'A'
  pp FB = putChar 'B'
  pp FC = putChar 'C'
  pp FD = putChar 'D'
  pp FE = putChar 'E'
  pp FF = putChar 'F'
  pp FG = putChar 'G'
  pp FH = putChar 'H'
 
instance PrettyPrint Piece where
  pp (Piece White k) = (putChar $ toUpper $ getPieceKindChar k)
  pp (Piece Black k) = (putChar $ toLower $ getPieceKindChar k)

instance PrettyPrint SquareState where
  pp Nothing = putChar '.' >> putChar ' '
  pp (Just p) = pp p >> putChar ' '

putRank :: BoardState -> Rank -> IO ()
putRank board rank = do
  pp rank >> putChar ' '
  let idxes = [(f, rank) | f <- [minBound..] ::[File]]
  mapM_ pp $ map (board !) idxes

instance PrettyPrint BoardState where
  pp board = do
    mapM_ (\r -> putRank board r >> putStrLn "") (reverse [minBound..] :: [Rank])
    putStr "  "
    mapM_ (\f -> pp f >> putChar ' ') ([minBound..] :: [File])
    putStrLn ""
