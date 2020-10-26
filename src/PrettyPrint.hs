{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint (
  PrettyPrint,
  pp
) where
import Data.Char
import qualified EnPassant as EP
import qualified CastlingRights as CR
import qualified Data.TotalMap as TM
import Piece
import Data.List
import Board
import Locus
import Game
import Move
import Evaluate

class PrettyPrint a where
  pp :: a -> String

getPieceKindChar :: PieceKind -> Char
getPieceKindChar Pawn   = 'p'
getPieceKindChar Rook   = 'r'
getPieceKindChar Bishop = 'b'
getPieceKindChar Knight = 'n'
getPieceKindChar King   = 'k'
getPieceKindChar Queen  = 'q'

instance PrettyPrint Rank where
  pp x =  pure $ case x of
    R8 -> '8'
    R7 -> '7'
    R6 -> '6'
    R5 -> '5'
    R4 -> '4'
    R3 -> '3'
    R2 -> '2'
    R1 -> '1'

instance PrettyPrint File where
  pp x = pure $ case x of
    FA-> 'a'
    FB-> 'b'
    FC-> 'c'
    FD-> 'd'
    FE-> 'e'
    FF-> 'f'
    FG-> 'g'
    FH-> 'h'

instance PrettyPrint Colour where
  pp = show

instance PrettyPrint Locus where
  pp l = case locToFR l of
    (file, rank) -> pp file ++ pp rank
 
instance PrettyPrint Piece where
  pp (Piece White k) = pure $ toUpper $ getPieceKindChar k
  pp (Piece Black k) = pure $ toLower $ getPieceKindChar k

instance PrettyPrint SquareState where
  pp Nothing = ['.']
  pp (Just p) = pp p

instance PrettyPrint (TM.TMap CR.CastlingRight Bool) where
  pp crMap = intercalate "\n" $ map (\cr -> show cr ++ ": " ++ show (crMap TM.! cr)) rights
    where rights = CR.CastlingRight <$> [CR.QueenSide,CR.KingSide] <*> [White,Black]


getRank :: BoardState -> Rank -> String
getRank b rank = do
  let squares = map (\l -> pp $ b ! frToLoc l) $ [(f, rank) | f <- [minBound..] ::[File]]
  intercalate " " $ (pp rank):squares

instance PrettyPrint EP.EnPassant where
  pp (EP.EnPassant Nothing) = ['-']
  pp (EP.EnPassant (Just (file,c))) = pp file >> pp rank
    where rank = if c == White then R3 else R6

instance PrettyPrint BoardState where
  pp b = intercalate "\n" l
    where ranks = map (getRank b) (reverse [minBound..] :: [Rank])
          files = "  " ++ (intercalate " " . map pp $ ([minBound..] :: [File]))
          l     = ranks ++ [files]

instance PrettyPrint GameState where
  pp g = intercalate "\n" x
    where x = [pp . board $ g
              , "Next to move: " ++ (pp . toMove $ g)
              ,  pp . castlingRights $ g
              , "EnPassant Locus: " ++ (pp . enPassant $ g)
              , "Evaluation: " ++ (show . evaluate $ g) ]

instance PrettyPrint Move where
  pp (Move src dst _ p) = concat [
      pp src
    , pp dst
    , promo ]
    where promo = case p of
            Just k -> pure . getPieceKindChar $ k
            Nothing -> ""
