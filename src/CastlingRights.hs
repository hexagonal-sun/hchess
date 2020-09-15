module CastlingRights (
  CastlingRights
, CastlingRight(..)
, CastlingSide(..)
, update
, defaultState
, create
) where

import qualified Data.TotalMap as TM
import Data.Array
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
updateCastlingRightsCapture board move = case board ! to move of
  Just (Piece c Rook) -> mapMaybe crFromLocus [to move]
  _                   -> []

updateCastlingRightsMove :: BoardState -> Move -> [CastlingRight]
updateCastlingRightsMove board move = case board ! from move of
  Just (Piece c King) -> [CastlingRight side c | side <- [QueenSide,KingSide]]
  Just (Piece c Rook) -> case fst $ locToFR $ from move of
    FA -> [CastlingRight QueenSide c]
    FH -> [CastlingRight KingSide  c]
    _  -> []
  _  -> []

update :: CastlingRights -> BoardState -> Move -> CastlingRights
update cr board move = foldr (`TM.insert` False) cr nullCr
  where nullCr = updateCastlingRightsMove board move ++ updateCastlingRightsCapture board move

defaultState :: CastlingRights
defaultState = TM.empty False
 
create :: [CastlingRight] -> CastlingRights
create []     = TM.empty False
create (r:rs) = TM.insert r True $ create rs
