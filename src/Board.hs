module Board (
  SquareState,
  BoardState,
  emptyBoard,
  validLocaii,
  startingBoard,
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

mkBoard :: (Locus -> SquareState) -> BoardState
mkBoard f =  emptyBoard // [(i, f i) | i <- validLocaii]

backRank :: File -> PieceKind
backRank f = case f of
  FA -> Rook
  FB -> Knight
  FC -> Bishop
  FD -> Queen
  FE -> King
  FF -> Bishop
  FG -> Knight
  FH -> Rook

startingBoardPieceGen :: Locus -> SquareState
startingBoardPieceGen l = case locToFR l of
  (_, R2)    -> Just (Piece White Pawn)
  (_, R7)    -> Just (Piece Black Pawn)
  (file, R1) -> Just (Piece White $ backRank file)
  (file, R8) -> Just (Piece Black $ backRank file)
  (_, _)     -> Nothing

startingBoard :: BoardState
startingBoard = mkBoard startingBoardPieceGen
