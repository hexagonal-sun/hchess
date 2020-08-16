module MoveGen (
  pseudoMoveGen
) where

import Data.Array
import Data.Maybe
import Data.List
import Control.Monad
import Board
import Locus
import Piece

data PieceDirection = PieceDirection [[Direction]] Bool

orthoDirs :: [[Direction]]
orthoDirs = [[North], [East], [South], [West]]

diagDirs :: [[Direction]]
diagDirs = [[North, East], [North, West],
            [South, East], [South, West]]

kindDirections' :: Locus -> Piece -> PieceDirection
kindDirections' (_, R2) (Piece White Pawn) = PieceDirection [[North, North], [North]] False
kindDirections' _       (Piece White Pawn) = PieceDirection [[North]] False
kindDirections' (_, R7) (Piece Black Pawn) = PieceDirection [[South], [South, South]] False
kindDirections' _       (Piece Black Pawn) = PieceDirection [[South]] False
kindDirections' _       (Piece _ Knight)   = PieceDirection [[North, North, East], [North, North, West],
                                                             [South, South, East], [South, South, West]]
                                                            False
kindDirections' _       (Piece _ King)     = PieceDirection orthoDirs False
kindDirections' _       (Piece _ Rook)     = PieceDirection orthoDirs True
kindDirections' _       (Piece _ Bishop)   = PieceDirection diagDirs True
kindDirections' _       (Piece _ Queen)    = PieceDirection (orthoDirs ++ diagDirs) True

kindDirections :: Locus -> Maybe Piece -> PieceDirection
kindDirections l (Just p) = kindDirections' l p
kindDirections _ Nothing  = PieceDirection [] False

applyDirections' :: Maybe Locus -> [Direction] -> [Maybe Locus]
applyDirections' Nothing _ = []
applyDirections' (Just l) dir = x:(applyDirections' x dir)
  where x = move l dir

applyDirections :: Locus -> [Direction] -> [Locus]
applyDirections l d = catMaybes $ applyDirections' (Just l) d

getMoves :: Locus -> PieceDirection -> [Locus]
getMoves l (PieceDirection dirs False) = catMaybes $ map (\d -> move l d) dirs
getMoves l (PieceDirection dirs True)  = concat $ map (\d -> applyDirections l d) dirs

pseudoMoveGen' :: BoardState -> Locus -> Locus -> BoardState
pseudoMoveGen' board from to = board // [(from, Nothing),
                                         (to,   board ! from)]


pseudoMoveGen :: BoardState -> Locus -> [BoardState]
pseudoMoveGen board l = map (pseudoMoveGen' board l) moves
  where pd = kindDirections l $ board ! l
        moves = getMoves l pd
