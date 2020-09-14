module MoveGen (
  moveGen
) where

import Data.Maybe
import qualified Data.TotalMap as TM
import qualified EnPassant as EP
import qualified CastlingRights as CR
import Data.Array
import Board
import Game
import Locus
import Piece
import Move

data RaySpec = RaySpec [Vector] Int

data MovementSpec = DifferingAttack RaySpec RaySpec |
                    ConsistentAttack RaySpec

data CandidateMoves = AttackOnly  [Ray] |
                      MoveOnly    [Ray] |
                      AttackMove  [Ray]

orthoVecs :: [Vector]
orthoVecs = [[North], [East], [South], [West]]

diagVecs :: [Vector]
diagVecs = [[North, East], [North, West],
            [South, East], [South, West]]

kindVectors :: Locus -> Piece -> MovementSpec
kindVectors (_, rank) (Piece c Pawn) = DifferingAttack moveVec attackVec
  where dir = if c == White then North else South
        atHome = case (rank,c) of
          (R7,Black) -> True
          (R2,White) -> True
          _          -> False
        moveVec = RaySpec [[dir]] $ if atHome then 2 else 1
        attackVec = RaySpec [[dir,East],[dir,West]] 1

kindVectors _ (Piece _ Knight)   = ConsistentAttack ray
  where ray = RaySpec [[North, North, East],  [North, North, West],
                       [South, South, East], [South, South, West],
                       [East, East, North],  [East, East, South],
                       [West, West, North],  [West, West, South]] 1

kindVectors _ (Piece _ King)     = ConsistentAttack ray

  where ray = RaySpec (orthoVecs ++ diagVecs) 1
kindVectors _ (Piece _ Rook)     = ConsistentAttack ray
  where ray = RaySpec orthoVecs repeatEntireSpan
kindVectors _ (Piece _ Bishop)   = ConsistentAttack ray
  where ray = RaySpec diagVecs repeatEntireSpan
kindVectors _ (Piece _ Queen)    = ConsistentAttack ray
  where ray = RaySpec (orthoVecs ++ diagVecs) repeatEntireSpan

getRays :: Locus -> Piece -> [CandidateMoves]
getRays l p = case kindVectors l p of
  ConsistentAttack (RaySpec vecs repeatVec) -> [AttackMove $ map (applyVector l repeatVec) vecs]
  DifferingAttack (RaySpec mVecs mRepeatVec) (RaySpec aVecs aRepeatVec) ->
    [AttackOnly $ map(applyVector l aRepeatVec) aVecs,
     MoveOnly   $ map(applyVector l mRepeatVec) mVecs]

canAttack :: GameState -> Colour -> Locus -> Bool
canAttack game c l = EP.isEPLocus (enPassant game) l || attackablePiece
  where
    attackablePiece = case board game ! l of
      Nothing -> False
      Just (Piece pc _) -> pc /= c

pruneAttackRay :: GameState -> Colour -> Ray -> Ray
pruneAttackRay game c ray = filter (canAttack game c) ray

isOccupied :: GameState -> Locus -> Bool
isOccupied game l = case board game ! l of
  Nothing -> False
  Just _ -> True

pruneMoveRay :: GameState -> Ray -> Ray
pruneMoveRay game ray = takeWhile (not . isOccupied game) ray

pruneAttackMoveRay :: GameState -> Colour -> Ray -> Ray
pruneAttackMoveRay _ _ []  = []
pruneAttackMoveRay game c (l:ls) = case board game ! l of
  Nothing                    -> l:pruneAttackMoveRay game c ls
  Just (Piece otherColour _) -> [l | otherColour /= c]

pruneMoves :: GameState -> Colour -> [CandidateMoves] -> Ray
pruneMoves game c = concatMap (\cm -> case cm of
  (AttackOnly rays) -> concatMap (pruneAttackRay game c) rays
  (MoveOnly rays)   -> concatMap (pruneMoveRay game) rays
  (AttackMove rays) -> concatMap (pruneAttackMoveRay game c) rays)

isRayAttacking' :: BoardState -> Piece -> Ray -> Bool
isRayAttacking' _ _ []    = False
isRayAttacking' b p@(Piece c k) (l:r) = case b ! l of
  Nothing -> isRayAttacking' b p r
  Just (Piece c' k') ->  c' == switch c && k' == k

isRayAttacking :: BoardState -> Piece -> CandidateMoves -> Bool
isRayAttacking _ _ (MoveOnly _)      = False
isRayAttacking b p (AttackOnly rays) = any (isRayAttacking' b p) rays
isRayAttacking b p (AttackMove rays) = any (isRayAttacking' b p) rays

isSquareUnderAttack' :: GameState -> Locus -> Piece -> Bool
isSquareUnderAttack' game l p = any (isRayAttacking (board game) p) $ getRays l p

isSquareUnderAttack :: GameState -> Colour -> Locus -> Bool
isSquareUnderAttack g c l = any (isSquareUnderAttack' g l . Piece (switch c)) allKinds

isInCheck :: Colour -> GameState -> Bool
isInCheck c game = isSquareUnderAttack game (switch c) kingPos
  where kingPos = if c == White then wKing game else bKing game

genCastlingMoves' :: GameState -> CR.CastlingRight -> Maybe Locus
genCastlingMoves' game (CR.CastlingRight side colour) =
  let dir      = if side == CR.QueenSide then West else East
      obsRaySz = if side == CR.QueenSide then 3 else 2
      rank     = if colour == White then R1 else R8
      from     = (FE,rank)
      obsRay   = applyVector from obsRaySz [dir]
      isOcc    = any (isOccupied game) obsRay
      checkRay = applyVector from 2 [dir]
      to       = last checkRay
      isCheck  = any (isSquareUnderAttack game (switch $ toMove game)) checkRay
  in if isOcc || isCheck then Nothing else Just to

genCastlingMoves :: GameState -> [Locus]
genCastlingMoves game | isInCheck (toMove game) game = []
                      | otherwise = mapMaybe (\cr -> if castlingRights game TM.! cr
                                               then genCastlingMoves' game cr
                                               else Nothing) $ [CR.CastlingRight side (toMove game) | side <- [CR.QueenSide,CR.KingSide]]

genPromotions :: Locus -> Locus -> Piece -> [Move]
genPromotions from to@(_, rank) (Piece c Pawn) | rank == R1 || rank == R8 = map (\pp -> Move from to (Just $ Piece c pp)) promotionKinds
genPromotions from to _ = [Move from to Nothing]

moveGen' :: GameState -> Locus -> [(Move, GameState)]
moveGen' game from = case board game ! from of
  Nothing -> []
  Just (Piece c _) | c /= toMove game -> []
  Just p@(Piece c k) -> mapMaybe (\move -> case makeMove game move of
                                     Nothing -> Nothing
                                     Just state -> Just (move, state)) moves
    where rays = getRays from p
          validMoves = pruneMoves game c rays ++ if k == King then genCastlingMoves game else []
          validMoves' = if k == King then filter (not . isSquareUnderAttack game (switch c)) validMoves else validMoves
          moves = concatMap (\to -> genPromotions from to p) validMoves'

moveGen :: GameState -> [(Move, GameState)]
moveGen game = filter (\(_,g) -> not $ isInCheck (toMove game) g) candidateMoves
  where candidateMoves = concatMap (moveGen' game) $ indices $ board game
