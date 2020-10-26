module Game (makeMove
            , GameState(..)
            ) where

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

updateBoard :: Piece -> Move -> BoardState -> BoardState
updateBoard (Piece c _) m@(Move _ _ _ (Just k)) b = b // [(from m, Nothing),
                                                        (to   m, Just (Piece c k))]
updateBoard _ m b = b // [(from m, Nothing),
                        (to   m, b ! from m)]

makeMove' :: GameState -> Piece -> Move -> BoardState
makeMove' game piece m = foldr (updateBoard piece) (board game) moves
  where m' = [m]
        fr = snd $ locToFR $ from m
        moves = case piece  of
          (Piece _ King) -> case (fst $ locToFR $ from m, fst $ locToFR $ to m) of
            (FE,FG) -> Move (frToLoc (FH,fr)) (frToLoc (FF,fr)) NotGenerated Nothing:m'
            (FE,FC) -> Move (frToLoc (FA,fr)) (frToLoc (FD,fr)) NotGenerated Nothing:m'
            _ -> m'
          (Piece _ Pawn) -> case EP.captureLoc (enPassant game) $ to m of
            Nothing -> m'
            Just capturedPawn -> Move (from m) capturedPawn NotGenerated Nothing:m'
          _ -> m'


makeMove :: GameState -> Move -> Maybe GameState
makeMove g@(GameState b nextColour ms wK bK _ _) m =
  case b ! from m of
    Nothing -> Nothing
    Just p@(Piece _ k)  -> Just $ GameState (makeMove' g p m) (switch nextColour) (m:ms) nwK nbK (EP.update k m) ncr
      where nwK = if from m == wK then to m else wK
            nbK = if from m == bK then to m else bK
            ncr = CR.update (castlingRights g) (board g) m
