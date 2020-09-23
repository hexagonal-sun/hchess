module Game (makeMove
            , GameState(..)
            , newGame
            ) where

import Data.Array
import qualified EnPassant as EP
import qualified CastlingRights as CR
import Board
import Locus
import Piece
import Move

data GameState = GameState
  { board :: BoardState
  , toMove  :: Colour
  , madeMoves :: [Move]
  , wKing :: Locus
  , bKing :: Locus
  , enPassant :: EP.EnPassant
  , castlingRights :: CR.CastlingRights }

updateBoard :: Move -> BoardState -> BoardState
updateBoard m@(Move _ _ (Just p)) b = b // [(from m, Nothing),
                                            (to   m, Just p)]
updateBoard m b = b // [(from m, Nothing),
                        (to   m, b ! from m)]

makeMove' :: GameState -> PieceKind -> Move -> BoardState
makeMove' game piece move = foldr updateBoard (board game) moves
  where m = [move]
        fr = snd $ locToFR $ from move
        moves = case piece of
          King -> case (fst $ locToFR $ from move, fst $ locToFR $ to move) of
            (FE,FG) -> Move (frToLoc (FH,fr)) (frToLoc (FF,fr)) Nothing:m
            (FE,FC) -> Move (frToLoc (FA,fr)) (frToLoc (FD,fr)) Nothing:m
            _ -> m
          Pawn -> case EP.captureLoc (enPassant game) $ to move of
            Nothing -> m
            Just capturedPawn -> Move (from move) capturedPawn Nothing:m
          _ -> m


makeMove :: GameState -> Move -> Maybe GameState
makeMove g@(GameState b nextColour moves wK bK _ cr) move =
  case b ! from move of
    Nothing -> Nothing
    Just (Piece c k)  -> Just $ GameState (makeMove' g k move) (switch nextColour) (move:moves) nwK nbK (EP.update k move) ncr
      where nwK = if from move == wK then to move else wK
            nbK = if from move == bK then to move else bK
            ncr = CR.update (castlingRights g) (board g) move

newGame :: GameState
newGame = GameState startingBoard White [] (frToLoc (FE,R1)) (frToLoc (FE,R8)) EP.defaultState CR.defaultState
