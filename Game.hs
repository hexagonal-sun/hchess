module Game (makeMove
            , CastlingSide(..)
            , CastlingRights(..)
            , GameState(..)
            , newGame
            ) where

import Data.Array
import qualified Data.TotalMap as TM
import Board
import Locus
import Piece

data CastlingSide = KingSide | QueenSide
  deriving(Show,Eq,Ord)

data CastlingRights = CastlingRights CastlingSide Colour
  deriving(Show,Eq,Ord)

data GameState = GameState
  { board :: BoardState
  , toMove  :: Colour
  , wKing :: Locus
  , bKing :: Locus
  , castlingRights :: TM.TMap CastlingRights Bool}
  deriving (Show)

createBoard :: BoardState -> Locus -> Locus -> BoardState
createBoard b from to = b // [(from, Nothing),
                              (to,   b ! from)]

makeMove' :: BoardState -> PieceKind -> Locus -> Locus -> BoardState
makeMove' b piece from@(ff,fr) to@(tf,_) = foldr (\(f,t) b' -> createBoard b' f t) b moves
  where m = [(from,to)]
        moves = case piece of
          King -> case (ff,tf) of
            (FE,FG) -> ((FH,fr),(FF,fr)):m
            (FE,FC) -> ((FA,fr),(FD,fr)):m
            _ -> m
          _ -> m

makeMove :: GameState -> Locus -> Locus -> Maybe GameState
makeMove (GameState b nextColour wK bK cr) from@(file,_) to =
  case b ! from of
    Nothing -> Nothing
    Just (Piece c k)  -> Just $ GameState (makeMove' b k from to) (switch nextColour) nwK nbK ncr
      where nwK = if from == wK then to else wK
            nbK = if from == bK then to else bK
            ncr = case k of
              King   -> foldr (`TM.insert` False) cr rights
                where rights = [CastlingRights side c | side <- [QueenSide,KingSide]]
              Knight -> case file  of
                FB -> TM.insert (CastlingRights QueenSide c) False cr
                FG -> TM.insert (CastlingRights KingSide  c) False cr
                _  -> cr
              _ -> cr

newGame :: GameState
newGame = GameState startingBoard White (FE,R1) (FE,R8) $ TM.empty True
