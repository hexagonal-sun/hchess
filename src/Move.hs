module Move (Move(..)) where

import Locus
import Piece

data Move = Move
  { from      :: Locus
  , to        :: Locus
  , promotion :: Maybe PieceKind}
  deriving(Eq, Show)
