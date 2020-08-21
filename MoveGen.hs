module MoveGen (
  moveGen
) where

import Data.Maybe
import Data.Array
import Board
import Game
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

pruneRay :: BoardState -> Colour -> Ray -> Ray
pruneRay _ _ []  = []
pruneRay board c (nl:ray) = case p of
                             Nothing -> nl:pruneRay board c ray
                             Just (Piece otherColour _) -> [nl | otherColour /= c]
                           where p = board ! nl

moveGen' :: GameState -> Locus -> SquareState -> [GameState]
moveGen' _ _ Nothing = []
moveGen' g@(GameState b _) l (Just p@(Piece c _)) = mapMaybe (makeMove g l) $ concat validMoves
  where movementSpec = kindVectors l p
        rays = getRays l movementSpec
        validMoves = map (pruneRay b c) rays

moveGen :: GameState -> Locus -> [GameState]
moveGen game@(GameState b _) l = moveGen' game l $ b ! l
