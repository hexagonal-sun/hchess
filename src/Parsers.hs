module Parsers(pMove) where

import Locus
import Move
import Piece
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Void ( Void )
import Data.Char

type Parser = Parsec Void String

pCharColour :: Char -> Parser Colour
pCharColour c = choice
  [ White <$ char (toUpper c)
  , Black <$ char c]

pPiece :: Parser Piece
pPiece = choice
    [ piece Pawn   <$> pCharColour 'p'
    , piece Rook   <$> pCharColour 'r'
    , piece Knight <$> pCharColour 'n'
    , piece Queen  <$> pCharColour 'q'
    , piece King   <$> pCharColour 'k'
    , piece Bishop <$> pCharColour 'b'] <?> "piece specifier"
  where
    piece = flip Piece

pRank :: Parser Rank
pRank = choice
  [ R1 <$ char '1'
  , R2 <$ char '2'
  , R3 <$ char '3'
  , R4 <$ char '4'
  , R5 <$ char '5'
  , R6 <$ char '6'
  , R7 <$ char '7'
  , R8 <$ char '8'] <?> "Board Rank"

pFile :: Parser File
pFile = choice
  [ FA <$ char 'a'
  , FB <$ char 'b'
  , FC <$ char 'c'
  , FD <$ char 'd'
  , FE <$ char 'e'
  , FF <$ char 'f'
  , FG <$ char 'g'
  , FH <$ char 'h'] <?> "Board File"

pLocus :: Parser Locus
pLocus = frToLoc <$> ((,) <$> pFile <*> pRank)

pMove :: Parser Move
pMove = Move <$> pLocus <*> pLocus <*> optional pPiece
