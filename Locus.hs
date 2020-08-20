module Locus (
  File(..), Rank(..), Locus, Direction(..), Vector, applyVector, Ray, move
) where

import Data.Ix
import Control.Monad
import Data.Maybe

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving(Eq, Ord, Ix, Bounded, Enum, Show)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving(Eq, Ord, Ix, Bounded, Enum, Show)

type Locus = (File, Rank)
 
data Direction = North | East | South | West
  deriving(Eq, Show)

type Vector = [Direction]
type Ray = [Locus]

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

move :: Locus -> Vector -> Maybe Locus
move = foldM move'

applyVector' :: Maybe Locus -> Vector -> [Maybe Locus]
applyVector' Nothing _ = []
applyVector' (Just l) dir = x:applyVector' x dir
  where x = move l dir

applyVector :: Locus -> Bool -> Vector -> Ray
applyVector l True v  = catMaybes $ applyVector' (Just l) v
applyVector l False v = case nl of
  Nothing -> []
  Just nl ->  [nl]
  where nl = move l v
