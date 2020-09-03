module Piece (
  PieceKind(..),
  allKinds,
  Piece(..),
  switch,
  Colour(..)
) where

data PieceKind = Pawn | Rook | Bishop | Knight | Queen | King
  deriving(Eq, Show)

allKinds :: [PieceKind]
allKinds = [Pawn, Rook, Bishop, Knight, Queen, King]

data Colour = White | Black
  deriving(Show, Eq, Ord)

switch :: Colour -> Colour
switch White = Black
switch Black = White

data Piece = Piece Colour PieceKind
  deriving(Show, Eq)
