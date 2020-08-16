module Locus (
  File(..), Rank(..), Locus, Direction(..), move
) where

import Data.Ix
import Control.Monad

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving(Eq, Ord, Ix, Bounded, Enum)

instance Show File where
  show FA = "a"
  show FB = "b"
  show FC = "c"
  show FD = "d"
  show FE = "e"
  show FF = "f"
  show FG = "g"
  show FH = "h"

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving(Eq, Ord, Ix, Bounded, Enum)

instance Show Rank where
  show R1 = "1"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"

type Locus = (File, Rank)
 
data Direction = North | East | South | West
  deriving(Eq, Show)

move' :: Locus -> Direction -> Maybe Locus
move' (file, rank) direction
  | direction == North && rank == (maxBound :: Rank) = Nothing
  | direction == South && rank == (minBound :: Rank) = Nothing
  | direction == East  && file == (maxBound :: File) = Nothing
  | direction == West  && file == (minBound :: File) = Nothing
move' (file, rank) North = Just (file, succ rank)
move' (file, rank) East  = Just (succ file, rank)
move' (file, rank) South = Just (file, pred rank)
move' (file, rank) West  = Just (pred file, rank)

move :: Locus -> [Direction] -> Maybe Locus
move start dirs = foldM move' start dirs
