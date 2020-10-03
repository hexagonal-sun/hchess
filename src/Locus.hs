module Locus (
  File(..),
  Rank(..),
  Locus,
  locToRank,
  locToFile,
  locToFR,
  frToLoc,
  north,
  east,
  south,
  west,
  Vector,
  applyVector,
  Ray,
  move,
  repeatEntireSpan
) where

import Data.Maybe
import Data.Word
import Data.Bits
import Data.Int

data File = FA | FB | FC | FD | FE | FF | FG | FH
  deriving(Eq, Ord, Bounded, Enum, Show)

data Rank = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving(Eq, Ord, Bounded, Enum, Show)

type Locus = Word8
type Vector = Int8

locToFile :: Locus -> File
locToFile l = toEnum $ fromIntegral $ l .&. 7

locToRank :: Locus -> Rank
locToRank l = toEnum $ fromIntegral $ l `shiftR` 4

locToFR :: Locus -> (File, Rank)
locToFR l = (locToFile l, locToRank l)

frToLoc :: (File, Rank) -> Locus
frToLoc (file, rank) = fromIntegral $ (16 * fromEnum rank) + fromEnum file

north :: Vector
north = 0x10

south :: Vector
south = -0x10

east :: Vector
east = 0x1

west :: Vector
west = -0x1

type Ray = [Locus]

move :: Locus -> Vector -> Maybe Locus
move l dir = if nLoc .&. 0x88 /= 0 then Nothing else Just nLoc
  where nLoc = l + fromIntegral dir

applyVector' :: Maybe Locus -> Int -> Vector -> [Maybe Locus]
applyVector' _ 0 _ = []
applyVector' Nothing _ _ = []
applyVector' (Just l) n dir = x:applyVector' x (n-1) dir
  where x = move l dir

applyVector :: Locus -> Int -> Vector -> Ray
applyVector l n v  = catMaybes $ applyVector' (Just l) n v

repeatEntireSpan :: Int
repeatEntireSpan = 8
