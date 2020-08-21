module Piece (
  PieceKind(..),
  Piece(..),
  Colour(..)
) where

data PieceKind = Pawn | Rook | Bishop | Knight | Queen | King
  deriving(Eq, Show)

data Colour = White | Black
  deriving(Show, Eq)

data Piece = Piece Colour PieceKind
  deriving(Show)
