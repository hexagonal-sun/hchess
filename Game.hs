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
import Move

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

updateBoard :: Move -> BoardState -> BoardState
updateBoard m b = b // [(from m, Nothing),
                        (to   m, b ! from m)]

makeMove' :: GameState -> PieceKind -> Move -> BoardState
makeMove' game piece move = foldr updateBoard (board game) moves
  where m = [move]
        m' =  case EP.captureLoc (enPassant game) $ to move of
          Nothing -> m
          Just capturedPawn -> Move (from move) capturedPawn Nothing:m
        fr = snd $ from move
        moves = case piece of
          King -> case (fst $ from move, fst $ to move) of
            (FE,FG) -> Move (FH,fr) (FF,fr) Nothing:m'
            (FE,FC) -> Move (FA,fr) (FD,fr) Nothing:m'
            _ -> m'
          _ -> m'

makeMove :: GameState -> Move -> Maybe GameState
makeMove g@(GameState b nextColour wK bK _ cr) move =
  case b ! from move of
    Nothing -> Nothing
    Just (Piece c k)  -> Just $ GameState (makeMove' g k move) (switch nextColour) nwK nbK (EP.update k move) ncr
      where nwK = if from move == wK then to move else wK
            nbK = if from move == bK then to move else bK
            ncr = case k of
              King   -> foldr (`TM.insert` False) cr rights
                where rights = [CastlingRights side c | side <- [QueenSide,KingSide]]
              Knight -> case fst $ from move of
                FB -> TM.insert (CastlingRights QueenSide c) False cr
                FG -> TM.insert (CastlingRights KingSide  c) False cr
                _  -> cr
              _ -> cr

newGame :: GameState
newGame = GameState startingBoard White (FE,R1) (FE,R8) EP.defaultState $ TM.empty True
