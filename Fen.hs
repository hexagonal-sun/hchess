{-# LANGUAGE TupleSections #-}

module Fen (parseFen) where

import Data.Array
import Data.Char
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.Except
import Control.Monad.Extra
import Piece
import Board
import Locus
import Game

data FenError = ParseError String
              | RowTooShort Rank
              | RowTooLong

instance Show FenError where
  show (ParseError s)  = "Error parsing FEN String:\n" ++ s
  show (RowTooShort r) = "Error Processing FEN String: Row specification too short at rank: " ++ show r
  show RowTooLong      = "Error Processing FEN String: Row specification too long"

type Parser = Parsec Void String
type FenMonad = Either FenError

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

createBoardRow :: [FENSpec] -> Maybe Locus ->  FenMonad [(Locus,SquareState)]
createBoardRow []      Nothing      = return []
createBoardRow _       Nothing      = throwError RowTooLong
createBoardRow []     (Just (_, r)) = throwError $ RowTooShort r
createBoardRow (s:xs) (Just l)      = case s of
  Left piece -> do
    np <- createBoardRow xs (move l [East])
    return $ (l,Just piece):np
  Right (FENSpace n) -> do
    let ray  = l:applyVector l (n - 1) [East]
    let next = move (last ray) [East]
    np <- createBoardRow xs next
    return $ map (,Nothing) ray ++ np

createBoard :: [[FENSpec]] -> FenMonad [(Locus,SquareState)]
createBoard s = do
  let start = (FA,R8)
      rows = start:applyVector start 7 [South]
      ls = zip rows s
  concatMapM (\(rs,spec) -> createBoardRow spec $ Just rs) ls

parseFen :: String -> FenMonad GameState
parseFen s = case parse pFen "f" s of
  Left r -> throwError $ ParseError $ errorBundlePretty r
  Right (FEN spec c) -> do
    boardInitaliser <- createBoard spec
    let board = array boardBounds boardInitaliser
    return $ GameState board c
