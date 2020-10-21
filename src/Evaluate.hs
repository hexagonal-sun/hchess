{-# LANGUAGE TemplateHaskell #-}
module Evaluate (evaluate) where

import Game
import Board
import Locus
import Data.Map (Map)
import Psqt
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Array as Array
import Piece

makePsqt

pieceMagMap :: Map PieceKind Double
pieceMagMap = Map.fromList [
    (Pawn, 100)
  , (Bishop, 300)
  , (Knight, 300)
  , (Rook, 500)
  , (Queen, 900)
  , (King, 0)
 ]

psqtValue :: Piece -> Locus -> Double
psqtValue p l = psqt p Vector.! (fromIntegral $ locToIdx l)

squareValue :: SquareState -> Locus -> Double
squareValue Nothing _ = 0
squareValue (Just p@(Piece c k)) l = if c == White then mag else negate mag
  where mag = pieceMagMap Map.! k + psqtValue p l

evaluate :: GameState -> Double
evaluate game = sum $ map (\l -> squareValue (board game Array.! l) l) validLocaii
