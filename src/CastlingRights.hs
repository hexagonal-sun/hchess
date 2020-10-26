module CastlingRights (
  CastlingRights
, CastlingRight(..)
, CastlingSide(..)
, update
, defaultState
, create
) where

import qualified Data.TotalMap as TM
import Data.Maybe
import Board
import Piece
import Move
import Locus

data CastlingSide = KingSide | QueenSide
  deriving(Show,Eq,Ord)

data CastlingRight = CastlingRight CastlingSide Colour
  deriving(Show,Eq,Ord)

type CastlingRights = TM.TMap CastlingRight Bool

csFromFile :: File -> Maybe CastlingSide
csFromFile file = case file of
  FA -> Just QueenSide
  FH -> Just KingSide
  _  -> Nothing

ccFromRank :: Rank -> Maybe Colour
ccFromRank rank = case rank of
  R1 -> Just White
  R8 -> Just Black
  _  -> Nothing

crFromLocus :: Locus -> Maybe CastlingRight
crFromLocus l = case locToFR l of
  (file, rank) -> CastlingRight <$> csFromFile file <*> ccFromRank rank

updateCastlingRightsCapture :: BoardState -> Move -> [CastlingRight]
updateCastlingRightsCapture board m = case board ! to m of
  SquareState (Just (Piece _ Rook)) -> mapMaybe crFromLocus [to m]
  SquareState (_)                   -> []

updateCastlingRightsMove :: BoardState -> Move -> [CastlingRight]
updateCastlingRightsMove board m = case board ! from m of
  SquareState (Just (Piece c King)) -> [CastlingRight side c | side <- [QueenSide,KingSide]]
  SquareState (Just (Piece c Rook)) -> case fst $ locToFR $ from m of
    FA -> [CastlingRight QueenSide c]
    FH -> [CastlingRight KingSide  c]
    _  -> []
  _  -> []

update :: CastlingRights -> BoardState -> Move -> CastlingRights
update cr board m = foldr (`TM.insert` False) cr nullCr
  where nullCr = updateCastlingRightsMove board m ++ updateCastlingRightsCapture board m

create :: [CastlingRight] -> CastlingRights
create = foldr (`TM.insert` True) (TM.empty False)

defaultState :: CastlingRights
defaultState = create [CastlingRight s c | s <- [KingSide, QueenSide],
                                           c <- [White, Black]]
