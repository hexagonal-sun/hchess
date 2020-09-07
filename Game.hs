module Game (makeMove
            , CastlingSide(..)
            , CastlingRights(..)
            , GameState(..)
            , newGame
            ) where

import Data.Array
import qualified Data.TotalMap as TM
import qualified EnPassant as EP
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
  , enPassant :: EP.EnPassant
  , castlingRights :: TM.TMap CastlingRights Bool}

createBoard :: BoardState -> Locus -> Locus -> BoardState
createBoard b from to = b // [(from, Nothing),
                              (to,   b ! from)]

makeMove' :: GameState -> PieceKind -> Locus -> Locus -> BoardState
makeMove' game piece from@(ff,fr) to@(tf,_) = foldr (\(f,t) b' -> createBoard b' f t) (board game) moves
  where m = [(from,to)]
        m' =  case EP.captureLoc (enPassant game) to of
          Nothing -> m
          Just capturedPawn -> (from,capturedPawn):m
        moves = case piece of
          King -> case (ff,tf) of
            (FE,FG) -> ((FH,fr),(FF,fr)):m'
            (FE,FC) -> ((FA,fr),(FD,fr)):m'
            _ -> m
          _ -> m

makeMove :: GameState -> Locus -> Locus -> Maybe GameState
makeMove g@(GameState b nextColour wK bK _ cr) from@(file,_) to =
  case b ! from of
    Nothing -> Nothing
    Just (Piece c k)  -> Just $ GameState (makeMove' g k from to) (switch nextColour) nwK nbK (EP.update k from to) ncr
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
newGame = GameState startingBoard White (FE,R1) (FE,R8) EP.defaultState $ TM.empty True
