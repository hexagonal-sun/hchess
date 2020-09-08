module MoveGen (
  moveGen
) where

import Data.Maybe
import qualified Data.TotalMap as TM
import qualified EnPassant as EP
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

isOccupied :: BoardState -> Maybe Locus -> Bool
isOccupied _ Nothing = False
isOccupied b (Just l) = case b ! l of
  Nothing -> False
  Just _ -> True

canAttack :: GameState -> Colour -> Maybe Locus -> Bool
canAttack _ _ Nothing = False
canAttack game c (Just l) = attackP || attackEP
  where attackEP = EP.isEPLocus (enPassant game) l
        attackP  = case (board game) ! l of
          Nothing -> False
          Just (Piece pC _) -> pC == switch c

kindVectors :: GameState -> Locus -> Piece -> MovementSpec
kindVectors game l@(_, rank) (Piece c Pawn) = MovementSpec (y:x) n
  where dir = if c == White then North else South
        moveOcc = isOccupied (board game) . move l
        moveAtt = canAttack game c . move l
        atHome = case (rank,c) of
          (R7,Black) -> True
          (R2,White) -> True
          _          -> False
        n = case atHome of
          True -> if moveOcc [dir,dir] then 1 else 2
          _    -> 1
        attackVecs = [[dir,East],[dir,West]]
        x = filter moveAtt attackVecs
        y = [dir | not $ moveOcc [dir]]

kindVectors _ _ (Piece _ Knight)   = MovementSpec [[North, North, East], [North, North, West],
                                                  [South, South, East], [South, South, West],
                                                  [East, East, North], [East, East, South],
                                                  [West, West, North], [West, West, South]]
                                   1
kindVectors _ _ (Piece _ King)     = MovementSpec (orthoVecs ++ diagVecs) 1
kindVectors _ _ (Piece _ Rook)     = MovementSpec orthoVecs repeatEntireSpan
kindVectors _ _ (Piece _ Bishop)   = MovementSpec diagVecs repeatEntireSpan
kindVectors _ _ (Piece _ Queen)    = MovementSpec (orthoVecs ++ diagVecs) repeatEntireSpan

getRays :: GameState -> Locus -> Piece -> [Ray]
getRays g l p = case kindVectors g l p of
  (MovementSpec vecs repeatVec) -> map (applyVector l repeatVec) vecs

pruneRay :: BoardState -> Colour -> Ray -> Ray
pruneRay _ _ []  = []
pruneRay b c (nl:ray) = case b ! nl of
                          Nothing -> nl:pruneRay b c ray
                          Just (Piece otherColour _) -> [nl | otherColour /= c]

isRayAttacking :: BoardState -> Piece -> Ray -> Bool
isRayAttacking _ _ [] = False
isRayAttacking b p (l:r)  = case b ! l of
  Nothing -> isRayAttacking b p r
  Just p' -> p == p'

isSquareUnderAttack' :: GameState -> Locus -> Piece -> Bool
isSquareUnderAttack' game l p = any (isRayAttacking (board game) p) $ getRays game l p

isSquareUnderAttack :: GameState -> Colour -> Locus -> Bool
isSquareUnderAttack g c l = any (isSquareUnderAttack' g l . Piece c) allKinds

isInCheck :: Colour -> GameState -> Bool
isInCheck c game = isSquareUnderAttack game (switch c) kingPos
  where kingPos = if c == White then wKing game else bKing game

genCastlingMoves' :: GameState -> CastlingRights -> Maybe Locus
genCastlingMoves' game (CastlingRights side colour) =
  let dir      = if side == QueenSide then West else East
      obsRaySz = if side == QueenSide then 3 else 2
      rank     = if colour == White then R1 else R8
      from     = (FE,rank)
      obsRay   = applyVector from obsRaySz [dir]
      isOcc    = any (isOccupied (board game) . Just) obsRay
      checkRay = applyVector from 2 [dir]
      to       = last checkRay
      isCheck  = any (isSquareUnderAttack game (switch $ toMove game)) checkRay
  in if isOcc || isCheck then Nothing else Just to

genCastlingMoves :: GameState -> [Locus]
genCastlingMoves game = mapMaybe (\cr -> if castlingRights game TM.! cr
                                   then genCastlingMoves' game cr
                                   else Nothing) $ [CastlingRights side (toMove game) | side <- [QueenSide,KingSide]]

moveGen' :: GameState -> Locus -> [(Locus, Locus, GameState)]
moveGen' game from = case board game ! from of
  Nothing -> []
  Just (Piece c _) | c /= toMove game -> []
  Just p@(Piece c k) -> mapMaybe (\to -> case makeMove game from to of
                                     Nothing -> Nothing
                                     Just state -> Just (from, to, state)) validMoves'
    where rays = getRays game from p
          validMoves = concatMap (pruneRay (board game) c) rays ++ if k == King then genCastlingMoves game else []
          validMoves' = if k == King then filter (not . isSquareUnderAttack game (switch c)) validMoves else validMoves

moveGen :: GameState -> [(Locus, Locus, GameState)]
moveGen game = filter (\(_,_,g) -> not $ isInCheck (toMove game) g) candidateMoves
  where candidateMoves = concatMap (moveGen' game) $ indices $ board game
