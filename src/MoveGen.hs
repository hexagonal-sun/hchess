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
  deriving(Show)

orthoVecs :: [Vector]
orthoVecs = [north, east, south, west]

diagVecs :: [Vector]
diagVecs = [north + east, north + west,
            south + east, south + west]

kindVectors :: Locus -> Piece -> MovementSpec
kindVectors l (Piece c Pawn) = DifferingAttack moveVec attackVec
  where dir = if c == White then north else south
        atHome = case (snd $ locToFR l,c) of
          (R7,Black) -> True
          (R2,White) -> True
          _          -> False
        moveVec = RaySpec [dir] $ if atHome then 2 else 1
        attackVec = RaySpec [dir + east, dir + west] 1

kindVectors _ (Piece _ Knight)   = ConsistentAttack ray
  where ray = RaySpec [north + north + east,  north + north + west,
                       south + south + east,  south + south + west,
                       east + east + north,   east + east + south,
                       west + west + north,   west + west + south] 1

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
pruneAttackRay game c = filter (canAttack game c)

isOccupied :: GameState -> Locus -> Bool
isOccupied game l = case board game ! l of
  Nothing -> False
  Just _ -> True

pruneMoveRay :: GameState -> Ray -> Ray
pruneMoveRay game = takeWhile (not . isOccupied game)

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
                      | otherwise = mapMaybe (\cr -> if castlingRights game TM.! cr
                                               then genCastlingMoves' game cr
                                               else Nothing) $ [CR.CastlingRight side (toMove game) | side <- [CR.QueenSide,CR.KingSide]]

genMoves :: Locus -> Locus -> Piece -> [Move]
genMoves src dst (Piece c Pawn) = case locToRank dst of
  rank | rank == R1 || rank == R8 -> map (\pp -> Move src dst (Just $ Piece c pp)) promotionKinds
       | otherwise                -> [Move src dst Nothing]
genMoves src dst _ = [Move src dst Nothing]

moveGen' :: GameState -> Locus -> [GameState]
moveGen' game src = case board game ! src of
  Nothing -> []
  Just (Piece c _) | c /= toMove game -> []
  Just p@(Piece c k) -> mapMaybe (makeMove game) moves
    where rays = getRays src p
          validMoves = pruneMoves game c rays ++ if k == King then genCastlingMoves game else []
          validMoves' = if k == King then filter (not . isSquareUnderAttack game (switch c)) validMoves else validMoves
          moves = concatMap (\dst -> genMoves src dst p) validMoves'

moveGen :: GameState -> [GameState]
moveGen game = filter (not . isInCheck (toMove game)) candidateMoves
  where candidateMoves = concatMap (moveGen' game) validLocaii
