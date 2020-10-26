{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Board (
  SquareState(..),
  BoardState,
  emptyBoard,
  (!),
  (//),
  validLocaii,
) where

import Locus
import Piece
import Data.Word
import Data.Tuple.Extra
import qualified Data.Vector.Storable as Vec
import Data.Bits ((.|.))
import Foreign.Ptr
import Foreign.Storable

newtype SquareState = SquareState (Maybe Piece)
  deriving(Eq,Show)

newtype BoardState  = BoardState (Vec.Vector SquareState)

instance Storable (SquareState) where
  sizeOf _    = 1
  alignment _ = 1
  poke ptr s = do
    let p = castPtr ptr :: Ptr Word8
    case s of
      SquareState (Nothing) -> poke p 0
      SquareState (Just piece)  -> poke p $ fromIntegral (fromEnum piece + 1)
  peek ptr = do
    let p = castPtr ptr :: Ptr Word8
    n <- peek p
    case n of
      0 -> return $ SquareState Nothing
      _ -> return $ SquareState (Just (toEnum . fromIntegral $ n - 1))

validLocaii :: [Locus]
validLocaii = (.|.) <$> [0..7] <*>  [0x0,0x10..0x70]

(!) :: BoardState -> Locus -> SquareState
(!) (BoardState vec) l = vec Vec.! fromIntegral l

(//) :: BoardState -> [(Locus, SquareState)] -> BoardState
(//) (BoardState vec) assoc = BoardState $ (vec Vec.// a')
  where a' = map (first fromIntegral) assoc

emptyBoard :: BoardState
emptyBoard = BoardState $ Vec.replicate 0x80 (SquareState Nothing)
