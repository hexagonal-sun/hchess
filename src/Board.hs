module Board (
  SquareState,
  BoardState,
  emptyBoard,
  (!),
  (//),
  validLocaii,
) where

import Locus
import Piece
import qualified Data.Vector as Vec
import Data.Bits
import Data.Tuple.Extra

type SquareState = Maybe Piece
newtype BoardState  = BoardState (Vec.Vector SquareState)

validLocaii :: [Locus]
validLocaii = (.|.) <$> [0..7] <*>  [0x0,0x10..0x70]

(!) :: BoardState -> Locus -> SquareState
(!) (BoardState vec) l = vec Vec.! fromIntegral l

(//) :: BoardState -> [(Locus, SquareState)] -> BoardState
(//) (BoardState vec) assoc = BoardState $ (vec Vec.// a')
  where a' = map (first fromIntegral) assoc

emptyBoard :: BoardState
emptyBoard = BoardState $ Vec.replicate 0x80 Nothing
