module MoveGen (
  moveGen
) where

import Data.Array
import Board
import Locus
import Piece

data MovementSpec = MovementSpec [Vector] Bool

orthoVecs :: [Vector]
orthoVecs = [[North], [East], [South], [West]]

diagVecs :: [Vector]
diagVecs = [[North, East], [North, West],
            [South, East], [South, West]]

kindVectors :: Locus -> Piece -> MovementSpec
kindVectors (_, R2) (Piece White Pawn) = MovementSpec [[North, North], [North]] False
kindVectors _       (Piece White Pawn) = MovementSpec [[North]] False
kindVectors (_, R7) (Piece Black Pawn) = MovementSpec [[South], [South, South]] False
kindVectors _       (Piece Black Pawn) = MovementSpec [[South]] False
kindVectors _       (Piece _ Knight)   = MovementSpec [[North, North, East], [North, North, West],
                                                        [South, South, East], [South, South, West]]
                                         False
kindVectors _       (Piece _ King)     = MovementSpec orthoVecs False
kindVectors _       (Piece _ Rook)     = MovementSpec orthoVecs True
kindVectors _       (Piece _ Bishop)   = MovementSpec diagVecs True
kindVectors _       (Piece _ Queen)    = MovementSpec (orthoVecs ++ diagVecs) True

getRays :: Locus -> MovementSpec -> [Ray]
getRays l (MovementSpec vecs repeatVec) = map (applyVector l repeatVec) vecs

createBoards :: BoardState -> Locus -> Locus -> BoardState
createBoards board from to = board // [(from, Nothing),
                                       (to,   board ! from)]

pruneRay :: BoardState -> Colour -> Ray -> Ray
pruneRay _ _ []  = []
pruneRay board c (nl:ray) = case p of
                             Nothing -> nl:pruneRay board c ray
                             Just (Piece otherColour _) -> [nl | otherColour /= c]
                           where p = board ! nl

moveGen' :: BoardState -> Locus -> SquareState -> [BoardState]
moveGen' _ _ Nothing = []
moveGen' b l (Just p@(Piece c _)) = map (createBoards b l) $ concat validMoves
  where movementSpec = kindVectors l p
        rays = getRays l movementSpec
        validMoves = map (pruneRay b c) rays

moveGen :: BoardState -> Locus -> [BoardState]
moveGen board l = moveGen' board l $ board ! l
