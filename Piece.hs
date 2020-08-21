module Piece (
  PieceKind(..),
  Piece(..),
  switch,
  Colour(..)
) where

data PieceKind = Pawn | Rook | Bishop | Knight | Queen | King
  deriving(Eq, Show)

data Colour = White | Black
  deriving(Show, Eq)

switch :: Colour -> Colour
switch White = Black
switch Black = White

data Piece = Piece Colour PieceKind
  deriving(Show)
