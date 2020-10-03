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
makeMove' game piece m = foldr updateBoard (board game) moves
  where m' = [m]
        fr = snd $ locToFR $ from m
        moves = case piece of
          King -> case (fst $ locToFR $ from m, fst $ locToFR $ to m) of
            (FE,FG) -> Move (frToLoc (FH,fr)) (frToLoc (FF,fr)) Nothing:m'
            (FE,FC) -> Move (frToLoc (FA,fr)) (frToLoc (FD,fr)) Nothing:m'
            _ -> m'
          Pawn -> case EP.captureLoc (enPassant game) $ to m of
            Nothing -> m'
            Just capturedPawn -> Move (from m) capturedPawn Nothing:m'
          _ -> m'


makeMove :: GameState -> Move -> Maybe GameState
makeMove g@(GameState b nextColour ms wK bK _ _) m =
  case b ! from m of
    Nothing -> Nothing
    Just (Piece _ k)  -> Just $ GameState (makeMove' g k m) (switch nextColour) (m:ms) nwK nbK (EP.update k m) ncr
      where nwK = if from m == wK then to m else wK
            nbK = if from m == bK then to m else bK
            ncr = CR.update (castlingRights g) (board g) m

newGame :: GameState
newGame = GameState startingBoard White [] (frToLoc (FE,R1)) (frToLoc (FE,R8)) EP.defaultState CR.defaultState
