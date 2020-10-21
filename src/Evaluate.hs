{-# LANGUAGE TemplateHaskell #-}
module Evaluate (evaluate) where

import Game
import Board
import Locus
import Data.Map (Map)
import Psqt
import MoveGen
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Array as Array
import Piece

makePsqt

inf :: Double
inf = read "Infinity"

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

terminalEval :: GameState -> Double
terminalEval g = if toMove g == White then negate mag else mag
  where mag = if isInCheck (toMove g) g then inf else 0

evaluate :: GameState -> Double
evaluate game = if (length . moveGen $ game) == 0 then terminalEval game else normalEval
  where normalEval   = sum $ map (\l -> squareValue (board game Array.! l) l) validLocaii
