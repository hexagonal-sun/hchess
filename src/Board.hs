module Board (
  SquareState,
  BoardState,
  emptyBoard,
  validLocaii,
) where
import Locus
import Piece
import Data.Array
import Data.Bits

type SquareState = Maybe Piece
type BoardState  = Array Locus SquareState

validLocaii :: [Locus]
validLocaii = (.|.) <$> [0..7] <*>  [0x0,0x10..0x70]

emptyBoard :: BoardState
emptyBoard = array (0, 0x7f) [(i, Nothing) | i <- [0..0x7f]]
