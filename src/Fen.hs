{-# LANGUAGE TupleSections #-}

module Fen
  ( FEN
  , pFen
  , parseFen
  , processFen
  , newGame ) where

import Data.Char
import Data.Void
import Text.Megaparsec hiding (State)
import qualified CastlingRights as CR
import Text.Megaparsec.Char
import Control.Monad.Except
import Control.Monad.Extra
import Piece
import Board
import Locus
import Game
import qualified EnPassant as EP

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

type FENSpec = Either Piece FENSpace
pRow :: Parser [FENSpec]
pRow = some $ (Left <$> pPiece) <|> (Right <$> pSpace)

pBoard :: Parser [[FENSpec]]
pBoard = (:) <$> pRow <*> count 7 (char '/' *> pRow)

pColour :: Parser Colour
pColour = choice
  [ White <$ char 'w'
  , Black <$ char 'b']

pCastlingRight :: Parser CR.CastlingRight
pCastlingRight = choice
  [ CR.CastlingRight CR.KingSide  <$> pCharColour 'k'
  , CR.CastlingRight CR.QueenSide <$> pCharColour 'q']

pCastlingRights :: Parser [CR.CastlingRight]
pCastlingRights = choice
  [ [] <$ char '-'
  , some pCastlingRight ]

data FEN = FEN [[FENSpec]] Colour [CR.CastlingRight]

pFen :: Parser FEN
pFen = do
  b <- pBoard
  space
  c <- pColour
  space
  FEN b c <$> pCastlingRights

createBoardRow :: [FENSpec] -> Maybe Locus ->  FenMonad [(Locus,SquareState)]
createBoardRow []      Nothing      = return []
createBoardRow _       Nothing      = throwError $ ProcessingError RowTooLong
createBoardRow []     (Just l) = throwError $ ProcessingError $ RowTooShort $ locToRank l
createBoardRow (s:xs) (Just l)      = case s of
  Left piece -> do
    np <- createBoardRow xs (move l east)
    return $ (l,Just piece):np
  Right (FENSpace n) -> do
    let ray  = l:applyVector l (n - 1) east
    let nextLocus = move (last ray) east
    np <- createBoardRow xs nextLocus
    return $ map (,Nothing) ray ++ np

createBoard :: [[FENSpec]] -> FenMonad [(Locus,SquareState)]
createBoard s = do
  let start = frToLoc (FA,R8)
      rows = start:applyVector start 7 south
      ls = zip rows s
  concatMapM (\(rs,spec) -> createBoardRow spec $ Just rs) ls

locateKing :: BoardState -> Colour -> FenMonad Locus
locateKing b c = case filter (\i -> (b ! i) == Just (Piece c King)) $ validLocaii of
  []  -> throwError $ ProcessingError $ NoKing c
  [x] -> return x
  _:_ -> throwError $ ProcessingError $ TooManyKings c

processFen :: FEN -> FenMonad GameState
processFen (FEN spec c cr) = do
   boardInitaliser <- createBoard spec
   let b = emptyBoard // boardInitaliser
   whiteKing <- locateKing b White
   blackKing <- locateKing b Black
   return $ GameState b c [] whiteKing blackKing EP.defaultState $ CR.create cr

parseFen :: String -> FenMonad GameState
parseFen s = case parse pFen "f" s of
  Left r -> throwError $ ParseError $ errorBundlePretty r
  Right fen -> processFen fen

newGame :: GameState
newGame = case parseFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq -" of
  Left _ -> error "Couldn't parse starting board fen string"
  Right g -> g
