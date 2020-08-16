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

dirRep :: [Direction] -> [[Direction]]
dirRep d = map (\n -> concat $ replicate n d) [1..7]

orthoDirs :: [[Direction]]
orthoDirs = dirRep [North] ++ dirRep[East] ++
            dirRep[South]  ++ dirRep[West]

diagDirs :: [[Direction]]
diagDirs = dirRep[North,East] ++ dirRep[North,West] ++
           dirRep[South,East] ++ dirRep[South,West]


kindDirections' :: Locus -> Piece -> [[Direction]]
kindDirections' (_, R2) (Piece White Pawn) = [[North, North], [North]]
kindDirections' _       (Piece White Pawn) = [[North]]
kindDirections' (_, R7) (Piece Black Pawn) = [[South], [South, South]]
kindDirections' _       (Piece Black Pawn) = [[South]]
kindDirections' _       (Piece _ Knight)   = [[North, North, East], [North, North, West],
                                              [South, South, East], [South, South, West]]
kindDirections' _       (Piece _ King)     = [[North], [East], [South], [West]]
kindDirections' _       (Piece _ Rook)     = orthoDirs
kindDirections' _       (Piece _ Bishop)   = diagDirs
kindDirections' _       (Piece _ Queen)    = orthoDirs ++ diagDirs

kindDirections :: Locus -> Maybe Piece -> [[Direction]]
kindDirections l (Just p) = kindDirections' l p
kindDirections _ Nothing  = []

pseudoMoveGen' :: BoardState -> Locus -> Locus -> BoardState
pseudoMoveGen' board from to = board // [(from, Nothing),
                                         (to,   board ! from)]

pseudoMoveGen :: BoardState -> Locus -> [BoardState]
pseudoMoveGen board l = map (pseudoMoveGen' board l) moves
  where directions = kindDirections l $ board ! l
        moves      = catMaybes $ map (move l) directions
