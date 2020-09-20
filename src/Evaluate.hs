
module Evaluate (evaluate) where

import Game
import Board
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Array
import Piece

pieceMagMap :: Map PieceKind Double
pieceMagMap = Map.fromList [
    (Pawn, 100)
  , (Bishop, 300)
  , (Knight, 300)
  , (Rook, 500)
  , (Queen, 900)
  , (King, 0)
 ]

pieceValue :: Piece -> Double
pieceValue (Piece c k) = if c == White then mag else negate mag
  where mag = pieceMagMap Map.! k

squareValue :: SquareState -> Double
squareValue Nothing = 0
squareValue (Just p)  = pieceValue p

evaluate :: GameState -> Double
evaluate game = sum $ map (squareValue . (board game !)) validLocaii
