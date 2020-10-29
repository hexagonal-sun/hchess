{-# LANGUAGE TemplateHaskell #-}

module MoveGen (
    moveGen
  , isInCheck
) where

import           Board
import qualified CastlingRights as CR
import           Data.List (sortOn)
import           Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import qualified EnPassant as EP
import           Game
import           Locus
import           Move
import           Piece
import           Rays

makeRays

canAttack :: GameState -> Colour -> Locus -> Bool
canAttack game c l = EP.isEPLocus (enPassant game) l || attackablePiece
  where
    attackablePiece = case board game ! l of
      SquareState(Nothing) -> False
      SquareState(Just (Piece pc _)) -> pc /= c

pruneAttackRay :: GameState -> Colour -> Ray -> Ray
pruneAttackRay game c = filter (canAttack game c)

isOccupied :: GameState -> Locus -> Bool
isOccupied game l = case board game ! l of
  SquareState(Nothing) -> False
  SquareState(Just _) -> True

pruneMoveRay :: GameState -> Ray -> Ray
pruneMoveRay game = takeWhile (not . isOccupied game)

pruneAttackMoveRay :: GameState -> Colour -> Ray -> Ray
pruneAttackMoveRay _ _ []  = []
pruneAttackMoveRay game c (l:ls) = case board game ! l of
  SquareState(Nothing)                    -> l:pruneAttackMoveRay game c ls
  SquareState(Just (Piece otherColour _)) -> [l | otherColour /= c]

pruneMoves :: GameState -> Colour -> [CandidateMoves] -> Ray
pruneMoves game c = concatMap (\cm -> case cm of
  (AttackOnly rays) -> concatMap (pruneAttackRay game c) rays
  (MoveOnly rays)   -> concatMap (pruneMoveRay game) rays
  (AttackMove rays) -> concatMap (pruneAttackMoveRay game c) rays)

isRayAttacking' :: BoardState -> Piece -> Ray -> Bool
isRayAttacking' _ _ []    = False
isRayAttacking' b p@(Piece c k) (l:r) = case b ! l of
  SquareState(Nothing) -> isRayAttacking' b p r
  SquareState(Just (Piece c' k')) ->  c' == switch c && k' == k

isRayAttacking :: BoardState -> Piece -> CandidateMoves -> Bool
isRayAttacking _ _ (MoveOnly _)      = False
isRayAttacking b p (AttackOnly rays) = any (isRayAttacking' b p) rays
isRayAttacking b p (AttackMove rays) = any (isRayAttacking' b p) rays

isSquareUnderAttack' :: GameState -> Locus -> Piece -> Bool
isSquareUnderAttack' game l p = any (isRayAttacking (board game) p) $ ((lookupRay p) Vec.! fromIntegral l)

isSquareUnderAttack :: GameState -> Colour -> Locus -> Bool
isSquareUnderAttack g c l = any (isSquareUnderAttack' g l . Piece (switch c)) allKinds

isInCheck :: Colour -> GameState -> Bool
isInCheck c game = isSquareUnderAttack game (switch c) kingPos
  where kingPos = if c == White then wKing game else bKing game

genCastlingMoves' :: GameState -> CR.CastlingRight -> Maybe Locus
genCastlingMoves' game (CR.CastlingRight side colour) =
  let dir      = if side == CR.QueenSide then west else east
      obsRaySz = if side == CR.QueenSide then 3 else 2
      rank     = if colour == White then R1 else R8
      src      = frToLoc (FE,rank)
      obsRay   = applyVector src obsRaySz dir
      isOcc    = any (isOccupied game) obsRay
      checkRay = applyVector src 2 dir
      dst      = last checkRay
      isCheck  = any (isSquareUnderAttack game (switch $ toMove game)) checkRay
  in if isOcc || isCheck then Nothing else Just dst

genCastlingMoves :: GameState -> [Locus]
genCastlingMoves game | isInCheck (toMove game) game = []
                      | otherwise = mapMaybe (\cr -> if castlingRights game Map.! cr
                                               then genCastlingMoves' game cr
                                               else Nothing) $ [CR.CastlingRight side (toMove game) | side <- [CR.QueenSide,CR.KingSide]]

genPromotions :: Locus -> Locus -> [Move]
genPromotions src dst = map (Move src dst Promotion . Just) promotionKinds

genMoves :: Locus -> Locus -> BoardState -> [Move]
genMoves src dst b = case (b ! src, locToRank dst) of
  (SquareState(Just (Piece _ Pawn)), rank) | rank == R1 || rank == R8 -> genPromotions src dst
  (_, _) -> case b ! dst of
    SquareState(Just _)  -> [Move src dst Capture Nothing]
    SquareState(Nothing) -> [Move src dst Quiet Nothing]

moveGen' :: GameState -> Locus -> [GameState]
moveGen' game src = case board game ! src of
  SquareState(Nothing) -> []
  SquareState(Just (Piece c _)) | c /= toMove game -> []
  SquareState(Just p@(Piece c k)) -> mapMaybe (makeMove game) moves
    where rays = lookupRay p Vec.! fromIntegral src
          validMoves = pruneMoves game c rays ++ if k == King then genCastlingMoves game else []
          validMoves' = if k == King then filter (not . isSquareUnderAttack game (switch c)) validMoves else validMoves
          moves = concatMap (\dst -> genMoves src dst $ board game) validMoves'

moveGen :: GameState -> [GameState]
moveGen game = filter (not . isInCheck (toMove game)) sortedMoves
  where candidateMoves = concatMap (moveGen' game) validLocaii
        sortedMoves = sortOn (kind . head . madeMoves) candidateMoves
