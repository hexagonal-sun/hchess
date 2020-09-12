module Piece (
  PieceKind(..),
  allKinds,
  Piece(..),
  switch,
  Colour(..)
) where

data PieceKind = Pawn | Rook | Bishop | Knight | Queen | King
  deriving(Eq, Show)

promotionKinds :: [PieceKind]
promotionKinds = [Rook, Bishop, Knight, Queen, King]

allKinds :: [PieceKind]
allKinds = Pawn:promotionKinds

data Colour = White | Black
  deriving(Show, Eq, Ord)

switch :: Colour -> Colour
switch White = Black
switch Black = White

data Piece = Piece Colour PieceKind
  deriving(Show, Eq)