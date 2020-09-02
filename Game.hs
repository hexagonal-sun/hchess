module Game (makeMove
            , GameState(..)
            , newGame
            ) where

import Data.Array
import Board
import Locus
import Piece

data GameState = GameState
  { board :: BoardState
  , next  :: Colour
  , wKing :: Locus
  , bKing :: Locus }
  deriving (Show)

createBoard :: BoardState -> Locus -> Locus -> BoardState
createBoard b from to = b // [(from, Nothing),
                              (to,   b ! from)]

makeMove :: GameState -> Locus -> Locus -> Maybe GameState
makeMove (GameState b nextColour wK bK) from to =
  case b ! from of
    Nothing -> Nothing
    Just _  -> Just $ GameState (createBoard b from to) (switch nextColour) nwK nbK
      where nwK = if from == wK then to else wK
            nbK = if from == bK then to else bK

newGame :: GameState
newGame = GameState startingBoard White (FE,R1) (FE,R8)
