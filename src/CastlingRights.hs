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
import Board
import Piece
import Move
import Locus

data CastlingSide = KingSide | QueenSide
  deriving(Show,Eq,Ord)

data CastlingRight = CastlingRight CastlingSide Colour
  deriving(Show,Eq,Ord)

type CastlingRights = TM.TMap CastlingRight Bool

updateCastlingRightsCapture :: BoardState -> Move -> [CastlingRight]
updateCastlingRightsCapture board move = case board ! to move of
  Just (Piece c Rook) | file == FA -> [CastlingRight QueenSide c]
                      | file == FH -> [CastlingRight KingSide  c]
    where file = fst $ to move
  _ -> []

updateCastlingRightsMove :: BoardState -> Move -> [CastlingRight]
updateCastlingRightsMove board move = case board ! from move of
  Just (Piece c King) -> [CastlingRight side c | side <- [QueenSide,KingSide]]
  Just (Piece c Rook) -> case fst $ from move of
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
