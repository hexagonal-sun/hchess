module Move (
    Move(..)
  , MoveKind(..)
) where

import Locus
import Piece

data MoveKind = Promotion | Check | Capture | Quiet | NotGenerated
  deriving(Eq,Ord,Show)

data Move = Move
  { from      :: Locus
  , to        :: Locus
  , kind      :: MoveKind
  , promotion :: Maybe PieceKind}
  deriving(Eq, Show)

