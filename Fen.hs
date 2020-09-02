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

data FenProcessingError = RowTooShort Rank
              | RowTooLong
              | NoKing Colour
              | TooManyKings Colour

data FenError = ParseError String
              | ProcessingError FenProcessingError

instance Show FenProcessingError where
  show (RowTooShort r) = "Error Processing FEN String: Row specification too short at rank: " ++ show r
  show RowTooLong      = "Row specification too long"
  show (NoKing c)      = "no " ++ show c ++ " king found"
  show (TooManyKings c)= "too many " ++ show c ++ " kings found"

instance Show FenError where
  show (ParseError s)      = "Error parsing FEN String:\n" ++ s
  show (ProcessingError e) = "Error processing FEN string: " ++ show e

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
createBoardRow _       Nothing      = throwError $ ProcessingError RowTooLong
createBoardRow []     (Just (_, r)) = throwError $ ProcessingError $ RowTooShort r
createBoardRow (s:xs) (Just l)      = case s of
  Left piece -> do
    np <- createBoardRow xs (move l [East])
    return $ (l,Just piece):np
  Right (FENSpace n) -> do
    let ray  = l:applyVector l (n - 1) [East]
    let nextLocus = move (last ray) [East]
    np <- createBoardRow xs nextLocus
    return $ map (,Nothing) ray ++ np

createBoard :: [[FENSpec]] -> FenMonad [(Locus,SquareState)]
createBoard s = do
  let start = (FA,R8)
      rows = start:applyVector start 7 [South]
      ls = zip rows s
  concatMapM (\(rs,spec) -> createBoardRow spec $ Just rs) ls

locateKing :: BoardState -> Colour -> FenMonad Locus
locateKing b c = case filter (\i -> (b ! i) == Just (Piece c King)) $ indices b of
  []  -> throwError $ ProcessingError $ NoKing c
  [x] -> return x
  _:_ -> throwError $ ProcessingError $ TooManyKings c

parseFen :: String -> FenMonad GameState
parseFen s = case parse pFen "f" s of
  Left r -> throwError $ ParseError $ errorBundlePretty r
  Right (FEN spec c) -> do
    boardInitaliser <- createBoard spec
    let b = array boardBounds boardInitaliser
    whiteKing <- locateKing b White
    blackKing <- locateKing b Black
    return $ GameState b c whiteKing blackKing