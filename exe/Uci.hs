module Uci (uciMain) where

import Move
import Fen
import Parsers
import Game
import Search
import PrettyPrint
import Perft
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Monad.State
import Data.Void ( Void )
import System.IO

newtype UciError = ParseError String

instance Show UciError where
  show (ParseError s) = "Error parsing UCI Command:\n" ++ s

type Parser = Parsec Void String
data PositionSpecifier = FENPos FEN | StartPos
type Uci = StateT GameState IO


data SearchType = BestMove | Perft Int

data UCICommand =
   UCI
 | IsReady
 | UCINewGame
 | Display
 | Position PositionSpecifier [Move]
 | Go SearchType

pPositionSpec :: Parser PositionSpecifier
pPositionSpec = choice
  [ StartPos <$ string "startpos",
    FENPos   <$ string "fen " <*> pFen]

pPerft :: Parser SearchType
pPerft = space1 >> string "perft" >> space1 >> Perft <$> decimal


pCommand :: Parser UCICommand
pCommand = choice
  [ IsReady    <$ string "isready"
  , UCINewGame <$ string "ucinewgame"
  , UCI        <$ string "uci"
  , Display    <$ string "d"
  , Position   <$ string "position " <*> pPositionSpec <*> many pMove
  , Go         <$ string "go" <*> (pPerft <|> pure BestMove) ]

applyMoves :: GameState -> [Move] -> Uci ()
applyMoves game moves = case foldM makeMove game moves of
  Just g -> put g
  Nothing -> lift $ putStrLn "Error making moves.  Possible illegal move."

handleCommand :: UCICommand -> Uci ()
handleCommand UCI = lift $ putStrLn $ "id name hchess\n" ++
                                      "id author Matthew Leach\n" ++
                                      "uciok"
handleCommand IsReady = lift $ putStrLn "readyok"
handleCommand Display = get >>= lift . pp
handleCommand UCINewGame = do
  put newGame
  lift $ putStrLn ""
handleCommand (Position StartPos moves) = applyMoves newGame moves
handleCommand (Position (FENPos fen) moves) = do
  case processFen fen of
    Left e -> lift $ print e
    Right game -> applyMoves game moves

handleCommand (Go BestMove) = do
  g <- get
  let move = search g 5
  lift $ putStr "bestmove "
  lift $ pp move
  lift $ putStrLn ""

handleCommand (Go (Perft n)) = do
  g <- get
  lift $ perftSplit g n

uciMain :: Uci ()
uciMain = do
  ln <- lift getLine
  case parse pCommand "<stdin>" ln of
    Left e -> lift $ putStrLn $ errorBundlePretty e
    Right command -> handleCommand command
  lift $ hFlush stdout
  uciMain
