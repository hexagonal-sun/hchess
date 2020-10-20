{-# LANGUAGE TemplateHaskell #-}

module Psqt (makePsqt) where

import Language.Haskell.TH
import Instances.TH.Lift()
import Language.Haskell.TH.Syntax
import qualified Data.Vector as Vector
import Piece

makePsqt' :: PatQ -> [Double] -> Q [Clause]
makePsqt' pt arr = do
  wPat  <- [p|Piece White $(pt)|]
  wBody <-[|$(lift $ Vector.fromList arr)|]
  bPat  <- [p|Piece Black $(pt)|]
  bBody <- [|$(lift $  Vector.fromList $ reverse arr)|]
  return $ [ Clause [wPat] (NormalB wBody) []
           , Clause [bPat] (NormalB bBody) []]


tables :: [[Double]]
tables =
  [
        [--A1                                  H1
          0,   0,   0,   0,   0,   0,   0,   0,
          10,  20,  0,   5,   5,   0,   20,  10,
          -10, 0,   5,   15,  15,  5,   0,   -10,
          -15, -5,  10,  25,  25,  10,  -5,  -15,
          -15, -5,  5,   15,  15,  5,   -5,  -15,
          45,  55,  65,  75,  75,  65,  55,  45,
          105, 115, 125, 135, 135, 125, 115, 105,
          0,   0,   0,   0,   0,   0,   0,   0
--       A8                                   H8
        ], [
         -50, -40, -30, -25, -25, -30, -40, -50,
         -35, -25, -15, -10, -10, -15, -25, -35,
         -20, -10,   0,   5,   5,   0, -10, -20,
         -10,   0,  10,  15,  15,  10,   0, -10,
         -5,   5,  15,  20,  20,  15,   5,  -5,
         -5,   5,  15,  20,  20,  15,   5,  -5,
         -20, -10,   0,   5,   5,   0, -10, -20,
         -135, -25, -15, -10, -10, -15, -25, -135
         ], [
         -2, -5, -16, -5, -5, -16, -5, -2,
         -5, 6,  3,   6,  6,  3,   6,  -5,
         -5, 5,  8,   10, 10, 8,   5,  -5,
         -5, 0,  10,  13, 13, 10,  0,  -5,
         -5, 0,  5,   13, 13, 5,   0,  -5,
         -5, 0,  5,   5,  5,  5,   0,  -5,
         -5, 0,  0,   0,  0,  0,   0,  -5,
         -5, -5, -5,  -5, -5, -5,  -5, -5
         ], [
          0,  1,  2,  4,  4,  2,  1,  0,
          -4, -2, 0,  2,  2,  0,  -2, -4,
          -4, -2, 0,  2,  2,  0,  -2, -4,
          -4, -2, 0,  2,  2,  0,  -2, -4,
          -4, -2, 0,  2,  2,  0,  -2, -4,
          -4, -2, 0,  2,  2,  0,  -2, -4,
          15, 15, 15, 15, 15, 15, 15, 15,
          4,  4,  4,  4,  4,  4,  4,  4
          ], [
          -5, -5, -5, -5, -5, -5, -5, -5,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0,
          0,  0,  0,  0,  0,  0,  0,  0
          ], [
          40,  50,  30,  10,  10,  30,  50,  40,
          30,  40,  20,  0,   0,   20,  40,  30,
          10,  20,  0,   -20, -20, 0,   20,  10,
          0,   10,  -10, -30, -30, -10, 10,  0,
          -10, 0,   -20, -40, -40, -20, 0,   -10,
          -20, -10, -30, -50, -50, -30, -10, -20,
          -30, -20, -40, -60, -60, -40, -20, -30,
          -40, -30, -50, -70, -70, -50, -30, -40
         ]
        ]

makePsqt :: Q [Dec]
makePsqt = do
  let kinds = [ [p|Pawn|] ,
                [p|Knight|] ,
                [p|Bishop|],
                [p|Rook|],
                [p|Queen|],
                [p|King|] ]

  let name = mkName "psqt"
  t <- [t|Piece -> Vector.Vector Double|]
  clauses <- sequence $ map (uncurry $ makePsqt') $ zip kinds tables
  return $ [SigD name t, FunD name $ concat clauses]
