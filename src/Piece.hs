module Piece (
  PieceKind(..),
  promotionKinds,
  allKinds,
  Piece(..),
  switch,
  Colour(..)
) where

import Data.Bits(shiftR, shiftL, (.&.))

data PieceKind = Pawn | Rook | Bishop | Knight | Queen | King
  deriving(Ord ,Eq, Show, Enum)

promotionKinds :: [PieceKind]
promotionKinds = [Rook, Bishop, Knight, Queen]

allKinds :: [PieceKind]
allKinds = [King,Pawn] ++ promotionKinds

data Colour = White | Black
  deriving(Show, Eq, Ord, Enum)

switch :: Colour -> Colour
switch White = Black
switch Black = White

data Piece = Piece Colour PieceKind
  deriving(Show, Eq)

instance Enum (Piece) where
  fromEnum (Piece c k) = (k' `shiftL` 1) + c'
    where c' = fromEnum c
          k' = fromEnum k
  toEnum n = Piece c k
    where c = toEnum $ n .&. 1
          k = toEnum $ n `shiftR` 1
