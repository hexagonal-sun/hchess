{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint (
  PrettyPrint,
  pp
) where
import Data.Char
import qualified EnPassant as EP
import qualified CastlingRights as CR
import Data.Array
import qualified Data.TotalMap as TM
import Piece
import Board
import Locus
import Game
import Move
import Evaluate

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
    FA-> 'a'
    FB-> 'b'
    FC-> 'c'
    FD-> 'd'
    FE-> 'e'
    FF-> 'f'
    FG-> 'g'
    FH-> 'h'

instance PrettyPrint Locus where
  pp l = case locToFR l of
    (file, rank) -> pp file >> pp rank
 
instance PrettyPrint Piece where
  pp (Piece White k) = putChar $ toUpper $ getPieceKindChar k
  pp (Piece Black k) = putChar $ toLower $ getPieceKindChar k

instance PrettyPrint SquareState where
  pp Nothing = putChar '.' >> putChar ' '
  pp (Just p) = pp p >> putChar ' '

instance PrettyPrint (TM.TMap CR.CastlingRight Bool) where
  pp crMap = mapM_ (\cr -> putStrLn $ show cr ++ ": " ++ show (crMap TM.! cr)) rights
    where rights = CR.CastlingRight <$> [CR.QueenSide,CR.KingSide] <*> [White,Black]


putRank :: BoardState -> Rank -> IO ()
putRank b rank = do
  pp rank >> putChar ' '
  let idxes = [(f, rank) | f <- [minBound..] ::[File]]
  mapM_ (\i -> pp $ b ! frToLoc i) idxes

instance PrettyPrint EP.EnPassant where
  pp (EP.EnPassant Nothing) = putChar '-'
  pp (EP.EnPassant (Just (file,c))) = pp file >> pp rank
    where rank = if c == White then R3 else R6

instance PrettyPrint BoardState where
  pp b = do
    mapM_ (\r -> putRank b r >> putStrLn "") (reverse [minBound..] :: [Rank])
    putStr "  "
    mapM_ (\f -> pp f >> putChar ' ') ([minBound..] :: [File])
    putStrLn ""

instance PrettyPrint GameState where
  pp g = do
    pp $ board g
    putStr "Next to move: "
    print $ toMove g
    pp $ castlingRights g
    putStr "EnPassant Locus: "
    pp $ enPassant g
    putStrLn ""
    putStr "Evaluation: "
    print $ evaluate g

instance PrettyPrint Move where
  pp (Move src dst promo) = do
    pp src
    pp dst
    case promo of
      Just (Piece c k) -> putChar $ getPieceKindChar k
      Nothing -> return ()
