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

data PieceVector = PieceVector [Vector] Bool

orthoVecs :: [Vector]
orthoVecs = [[North], [East], [South], [West]]

diagVecs :: [Vector]
diagVecs = [[North, East], [North, West],
            [South, East], [South, West]]

kindVectors' :: Locus -> Piece -> PieceVector
kindVectors' (_, R2) (Piece White Pawn) = PieceVector [[North, North], [North]] False
kindVectors' _       (Piece White Pawn) = PieceVector [[North]] False
kindVectors' (_, R7) (Piece Black Pawn) = PieceVector [[South], [South, South]] False
kindVectors' _       (Piece Black Pawn) = PieceVector [[South]] False
kindVectors' _       (Piece _ Knight)   = PieceVector [[North, North, East], [North, North, West],
                                                             [South, South, East], [South, South, West]]
                                                            False
kindVectors' _       (Piece _ King)     = PieceVector orthoVecs False
kindVectors' _       (Piece _ Rook)     = PieceVector orthoVecs True
kindVectors' _       (Piece _ Bishop)   = PieceVector diagVecs True
kindVectors' _       (Piece _ Queen)    = PieceVector (orthoVecs ++ diagVecs) True

kindVectors :: Locus -> Maybe Piece -> PieceVector
kindVectors l (Just p) = kindVectors' l p
kindVectors _ Nothing  = PieceVector [] False

applyVectors' :: Maybe Locus -> Vector -> [Maybe Locus]
applyVectors' Nothing _ = []
applyVectors' (Just l) dir = x:applyVectors' x dir
  where x = move l dir

applyVectors :: Locus -> Vector -> [Locus]
applyVectors l d = catMaybes $ applyVectors' (Just l) d

getMoves :: Locus -> PieceVector -> [Locus]
getMoves l (PieceVector dirs False) = mapMaybe (move l) dirs
getMoves l (PieceVector dirs True)  = concatMap (applyVectors l) dirs

pseudoMoveGen' :: BoardState -> Locus -> Locus -> BoardState
pseudoMoveGen' board from to = board // [(from, Nothing),
                                         (to,   board ! from)]


pseudoMoveGen :: BoardState -> Locus -> [BoardState]
pseudoMoveGen board l = map (pseudoMoveGen' board l) moves
  where pd = kindVectors l $ board ! l
        moves = getMoves l pd
