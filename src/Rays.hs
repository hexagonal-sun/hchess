{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}

module Rays (makeRays, CandidateMoves(..)) where

import Language.Haskell.TH
import Instances.TH.Lift()
import Language.Haskell.TH.Syntax
import Data.Maybe
import qualified Data.Vector as Vec
import Piece
import Locus
import Board

data RaySpec = RaySpec [Vector] Int

data MovementSpec = DifferingAttack RaySpec RaySpec |
                    ConsistentAttack RaySpec

data CandidateMoves = AttackOnly  [Ray] |
                      MoveOnly    [Ray] |
                      AttackMove  [Ray]
  deriving(Show,Lift)

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

getRays :: Piece -> Locus -> [CandidateMoves]
getRays p l = let  nn = filter (not . null)
                   in case kindVectors l p of
  ConsistentAttack (RaySpec vecs repeatVec) -> [AttackMove $ nn . map (applyVector l repeatVec) $ vecs]
  DifferingAttack (RaySpec mVecs mRepeatVec) (RaySpec aVecs aRepeatVec) ->
    [AttackOnly $ nn . map(applyVector l aRepeatVec) $ aVecs,
     MoveOnly   $ nn . map(applyVector l mRepeatVec) $ mVecs]

makeRayVec :: Piece -> Locus -> [CandidateMoves]
makeRayVec p l = if l `elem` $$(validLocaii) then getRays p l else []

makeRays' :: Piece -> Q [Clause]
makeRays' pt@(Piece c k) = do
  cName <- lookupValueName $ show c
  kName <- lookupValueName $ show k

  piecePat <- pure . ConP 'Piece $ [ConP (fromJust cName) [], ConP (fromJust kName) []]
  let allLocaii = map (makeRayVec pt) ([minBound..maxBound] :: [Locus])
 
  rays <- lift .  Vec.fromList $ allLocaii
  return $ [ Clause [piecePat] (NormalB rays) [] ]

makeRays :: Q [Dec]
makeRays = do
  let allPieces = map (uncurry Piece) [(b,a) | a <- allKinds, b <- [Black, White]]

  let name = mkName "lookupRay"
  t <- [t|Piece -> (Vec.Vector [CandidateMoves])|]
  clauses <- sequence $ map makeRays' allPieces
  return $ [SigD name t, FunD name $ concat clauses]
