module MoveGen (
  moveGen
) where

import Data.Maybe
import Data.Array
import Board
import Game
import Locus
import Piece

data MovementSpec = MovementSpec [Vector] Int

orthoVecs :: [Vector]
orthoVecs = [[North], [East], [South], [West]]

diagVecs :: [Vector]
diagVecs = [[North, East], [North, West],
            [South, East], [South, West]]

kindVectors :: Locus -> Piece -> MovementSpec
kindVectors (_, R2) (Piece White Pawn) = MovementSpec [[North]] 2
kindVectors _       (Piece White Pawn) = MovementSpec [[North]] 1
kindVectors (_, R7) (Piece Black Pawn) = MovementSpec [[South]] 2
kindVectors _       (Piece Black Pawn) = MovementSpec [[South]] 1
kindVectors _       (Piece _ Knight)   = MovementSpec [[North, North, East], [North, North, West],
                                                        [South, South, East], [South, South, West],
                                                        [East, East, North], [East, East, South],
                                                        [West, West, North], [West, West, South]]
                                         1
kindVectors _       (Piece _ King)     = MovementSpec orthoVecs 1
kindVectors _       (Piece _ Rook)     = MovementSpec orthoVecs repeatEntireSpan
kindVectors _       (Piece _ Bishop)   = MovementSpec diagVecs repeatEntireSpan
kindVectors _       (Piece _ Queen)    = MovementSpec (orthoVecs ++ diagVecs) repeatEntireSpan

getRays :: Locus -> Piece -> [Ray]
getRays l p = case kindVectors l p of
  (MovementSpec vecs repeatVec) -> map (applyVector l repeatVec) vecs

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
isSquareUnderAttack' b l p = any (isRayAttacking b p) $ getRays l p

isSquareUnderAttack :: BoardState -> Colour -> Locus -> Bool
isSquareUnderAttack b c l = any (isSquareUnderAttack' b l . Piece c) allKinds

moveGen' :: GameState -> Locus -> [(Locus, Locus, GameState)]
moveGen' g@(GameState b nc) from = case b ! from of
  Nothing -> []
  Just (Piece c _) | c /= nc -> []
  Just p@(Piece c k) -> mapMaybe (\to -> case makeMove g from to of
                                     Nothing -> Nothing
                                     Just state -> Just (from, to, state)) validMoves'
    where rays = getRays from p
          validMoves = concatMap (pruneRay b c) rays
          validMoves' = if k == King then filter (isSquareUnderAttack b (switch c)) validMoves else validMoves

moveGen :: GameState -> [(Locus, Locus, GameState)]
moveGen game@(GameState b _) = concatMap (moveGen' game) $ indices b
