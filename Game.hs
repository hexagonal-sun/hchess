module Game (makeMove
            , GameState(..)
            , newGame
            ) where

import Data.Array
import Board
import Locus
import Piece

data GameState = GameState BoardState Colour
  deriving (Show)

createBoard :: BoardState -> Locus -> Locus -> BoardState
createBoard board from to = board // [(from, Nothing),
                                      (to,   board ! from)]

makeMove :: GameState -> Locus -> Locus -> Maybe GameState
makeMove (GameState board nextColour) from to =
  case sq of
    Nothing           -> Nothing
    Just (Piece c _) -> if c == nextColour then
      Just $ GameState (createBoard board from to) $ switch nextColour
      else
      Nothing
  where sq = board ! from

newGame :: GameState
newGame = GameState startingBoard White
