{-# LANGUAGE DeriveLift #-}

module Locus (
  File(..),
  Rank(..),
  Locus(..),
  PseudoLocus(..),
  locToRank,
  locToFile,
  locToFR,
  frToLoc,
  plToLoc,
  locToPl,
  locToIdx,
  allLocaii,
  north,
  east,
  south,
  west,
  Vector,
  applyVector,
  PseudoRay,
  Ray,
  move,
  repeatEntireSpan
) where

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Int
import Language.Haskell.TH.Syntax

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving(Eq, Ord, Bounded, Enum, Show)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving(Eq, Ord, Bounded, Enum, Show)

type Vector = Int8
newtype PseudoLocus = PseudoLocus Word8
  deriving(Show, Lift)

newtype Locus = Locus Word8
  deriving(Eq, Show, Lift)

locToFile :: Locus -> File
locToFile (Locus l) = toEnum $ fromIntegral $ l .&. 7

locToRank :: Locus -> Rank
locToRank (Locus l) = toEnum $ fromIntegral $ l `shiftR` 3

locToFR :: Locus -> (File, Rank)
locToFR l = (locToFile l, locToRank l)

frToLoc :: (File, Rank) -> Locus
frToLoc (file, rank) = Locus $ fromIntegral $ (fromEnum rank `shiftL` 3) + fromEnum file

plToLoc :: PseudoLocus -> Locus
plToLoc (PseudoLocus pl) = Locus $ (rank `shiftL` 3) + file
  where file = toEnum . fromIntegral $ pl .&. 7
        rank = toEnum . fromIntegral $ pl `shiftR` 4

locToPl :: Locus -> PseudoLocus
locToPl l = PseudoLocus $ (rank `shiftL` 4) + file
  where file = fromIntegral . fromEnum . locToFile $ l
        rank = fromIntegral . fromEnum . locToRank $ l

locToIdx :: Locus -> Int
locToIdx (Locus l) = fromIntegral l

allLocaii :: [Locus]
allLocaii = Locus <$> [0..63]

north :: Vector
north = 0x10

south :: Vector
south = -0x10

east :: Vector
east = 0x1

west :: Vector
west = -0x1

type PseudoRay = [PseudoLocus]
type Ray       = [Locus]

move :: PseudoLocus -> Vector -> Maybe PseudoLocus
move (PseudoLocus pl) dir = if nLoc .&. 0x88 /= 0 then Nothing else Just (PseudoLocus nLoc)
  where nLoc = pl + fromIntegral dir

applyVector' :: Maybe PseudoLocus -> Int -> Vector -> [Maybe PseudoLocus]
applyVector' _ 0 _ = []
applyVector' Nothing _ _ = []
applyVector' (Just l) n dir = x:applyVector' x (n-1) dir
  where x = move l dir

applyVector :: PseudoLocus -> Int -> Vector -> PseudoRay
applyVector l n v  = catMaybes $ applyVector' (Just l) n v

repeatEntireSpan :: Int
repeatEntireSpan = 8
