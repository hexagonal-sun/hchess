module Board (
  SquareState,
  BoardState,
  startingBoard,
) where
import Locus
import Piece
import Data.Ix
import Data.Array


type SquareState = Maybe Piece
type BoardState  = Array (File, Rank) SquareState

mkArray :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds =  array bnds [(i, f i) | i <- range bnds]

boardBounds :: (Locus, Locus)
boardBounds = ((minBound :: File, minBound :: Rank),
               (maxBound :: File, maxBound :: Rank))

backRank :: File -> PieceKind
backRank FA = Rook
backRank FB = Knight
backRank FC = Bishop
backRank FD = Queen
backRank FE = King
backRank FF = Bishop
backRank FG = Knight
backRank FH = Rook

startingBoardPieceGen :: Locus -> SquareState
startingBoardPieceGen (_, R2) = Just (Piece White Pawn)
startingBoardPieceGen (_, R7) = Just (Piece Black Pawn)
startingBoardPieceGen (file, R1) = Just (Piece White $ backRank file)
startingBoardPieceGen (file, R8) = Just (Piece Black $ backRank file)
startingBoardPieceGen (_, _) = Nothing
  where br = King

startingBoard :: BoardState
startingBoard = mkArray startingBoardPieceGen boardBounds
