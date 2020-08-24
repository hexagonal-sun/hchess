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
                                                        [South, South, East], [South, South, West],
                                                        [East, East, North], [East, East, South],
                                                        [West, West, North], [West, West, South]]
                                         False
kindVectors _       (Piece _ King)     = MovementSpec orthoVecs False
kindVectors _       (Piece _ Rook)     = MovementSpec orthoVecs True
kindVectors _       (Piece _ Bishop)   = MovementSpec diagVecs True
kindVectors _       (Piece _ Queen)    = MovementSpec (orthoVecs ++ diagVecs) True

getRays :: Locus -> MovementSpec -> [Ray]
getRays l (MovementSpec vecs repeatVec) = map (applyVector l repeatVec) vecs

pruneRay :: BoardState -> Colour -> Ray -> Ray
pruneRay _ _ []  = []
pruneRay board c (nl:ray) = case board ! nl of
                             Nothing -> nl:pruneRay board c ray
                             Just (Piece otherColour _) -> [nl | otherColour /= c]

isRayAttacking :: BoardState -> Piece -> Ray -> Bool
isRayAttacking _ _ [] = False
isRayAttacking b p (l:r)  = case b ! l of
  Nothing -> isRayAttacking b p r
  Just p' -> p == p'

isSquareUnderAttack' :: BoardState -> Locus -> Piece -> Bool
isSquareUnderAttack' b l p = any (isRayAttacking b p) $ getRays l $ kindVectors l p

isSquareUnderAttack :: BoardState -> Colour -> Locus -> Bool
isSquareUnderAttack b c l = any (isSquareUnderAttack' b l . Piece c) allKinds

moveGen' :: GameState -> Locus -> [(Locus, Locus, GameState)]
moveGen' g@(GameState b nc) from = case b ! from of
  Nothing -> []
  Just (Piece c _) | c /= nc -> []
  Just p@(Piece c k) -> mapMaybe (\to -> case makeMove g from to of
                                     Nothing -> Nothing
                                     Just state -> Just (from, to, state)) validMoves'
    where movementSpec = kindVectors from p
          rays = getRays from movementSpec
          validMoves = concatMap (pruneRay b c) rays
          validMoves' = if k == King then filter (isSquareUnderAttack b (switch c)) validMoves else validMoves

moveGen :: GameState -> [(Locus, Locus, GameState)]
moveGen game@(GameState b _) = concatMap (moveGen' game) $ indices b
