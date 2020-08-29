{-# LANGUAGE TupleSections #-}

module Fen (parseFen) where

import Data.Array
import Data.Char
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Piece
import Board
import Locus
import Game

type Parser = Parsec Void String

newtype FENSpace = FENSpace Int deriving (Show)
pSpace :: Parser FENSpace
pSpace = oneOf ['1'..'8'] >>= (pure . FENSpace) . digitToInt <?> "space specifier"

pPiece :: Parser Piece
pPiece = choice
    [ piece Pawn   <$> charCaseClr 'p'
    , piece Rook   <$> charCaseClr 'r'
    , piece Knight <$> charCaseClr 'n'
    , piece Queen  <$> charCaseClr 'q'
    , piece King   <$> charCaseClr 'k'
    , piece Bishop <$> charCaseClr 'b'] <?> "piece specifier"
  where
    charCaseClr c = (char (toUpper c) >> return White) <|> (char c >> return Black) :: Parser Colour
    piece = flip Piece

type FENSpec = Either Piece FENSpace
pRow :: Parser [FENSpec]
pRow = some $ (Left <$> pPiece) <|> (Right <$> pSpace)

pBoard :: Parser [[FENSpec]]
pBoard = (:) <$> pRow <*> count 7 (char '/' *> pRow)

pColour :: Parser Colour
pColour = choice
  [ White <$ char 'w'
  , Black <$ char 'b']

data FEN = FEN [[FENSpec]] Colour
pFen :: Parser FEN
pFen = do
  b <- pBoard
  space
  FEN b <$> pColour

createBoardRow :: [FENSpec] -> Maybe Locus ->  [(Locus,SquareState)]
createBoardRow [] _ = []
createBoardRow _ Nothing = []
createBoardRow (s:xs) (Just l) = case s of
  Left piece -> (l,Just piece):createBoardRow xs (move l [East])
  Right (FENSpace n) -> map (,Nothing) ray ++ createBoardRow xs next
    where ray = l:applyVector l (n - 1) [East]
          next = move (last ray) [East]

createBoard :: [[FENSpec]] -> [(Locus,SquareState)]
createBoard s = concatMap (\(rs,spec) -> createBoardRow spec $ Just rs) ls
  where start = (FA,R8)
        rows = start:applyVector start 7 [South]
        ls = zip rows s

parseFen :: String -> Maybe GameState
parseFen s = do
  (FEN spec c) <- parseMaybe pFen s
  let board = array boardBounds $ createBoard spec
  return (GameState board c)
