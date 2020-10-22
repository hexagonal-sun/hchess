module Uci (uciMain) where

import Move
import Fen
import Parsers
import Game
import qualified Search as Search
import PrettyPrint
import Perft
import Piece
import Text.Megaparsec hiding (State)
import Data.List
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Monad.State
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Void ( Void )
import System.IO
import System.Exit (exitSuccess)
import Control.Exception.Base (evaluate)

newtype UciError = ParseError String

instance Show UciError where
  show (ParseError s) = "Error parsing UCI Command:\n" ++ s

type Parser = Parsec Void String
data PositionSpecifier = FENPos FEN | StartPos
type Uci = StateT GameState IO

data SearchType = BestMove [BestMoveParam] | Perft Int

data BestMoveParam =
    Time Colour Int
  | Increment Colour Int


data UCICommand =
   UCI
 | IsReady
 | UCINewGame
 | Display
 | Quit
 | Position PositionSpecifier [Move]
 | Go SearchType

pPositionSpec :: Parser PositionSpecifier
pPositionSpec = choice
  [ StartPos <$ string "startpos",
    FENPos   <$ string "fen " <*> pFen]

pMoveList :: Parser [Move]
pMoveList = do
  space1
  _ <- string "moves"
  space1
  sepBy1 pMove space1

pPerft :: Parser SearchType
pPerft = space1 >> string "perft" >> space1 >> Perft <$> decimal

pColour :: Parser Colour
pColour = Black <$ char 'b' <|> White <$ char 'w'

pTime :: Parser BestMoveParam
pTime = do
  space1
  c <- pColour
  _ <- string "time"
  space1
  n <- decimal
  return $ Time c n

pInc :: Parser BestMoveParam
pInc =  do
  space1
  c <- pColour
  _ <- string "inc"
  space1
  n <- decimal
  return $ Increment c n

pBMParams :: Parser [BestMoveParam]
pBMParams = many $ choice [try pTime, try pInc]

pCommand :: Parser UCICommand
pCommand = choice
  [ IsReady    <$ string "isready"
  , UCINewGame <$ string "ucinewgame"
  , UCI        <$ string "uci"
  , Display    <$ string "d"
  , Quit       <$ string "quit"
  , Position   <$ string "position " <*> pPositionSpec <*> option [] pMoveList
  , Go         <$ string "go" <*> (try pPerft <|> BestMove <$> pBMParams ) ]


calcTimeToSearch :: Colour -> [BestMoveParam] -> Int
calcTimeToSearch _ []      = 2000000
calcTimeToSearch c ((Time c' t):params)
  | c == c' = (t * 1000) `div` 30
  | otherwise = calcTimeToSearch c params
calcTimeToSearch c (_:params) = calcTimeToSearch c params

searchIterDeep' :: MVar (Maybe Move) -> GameState -> Int -> IO ()
searchIterDeep' mv g depth = do
  (score, pv) <- evaluate $ Search.search g depth
  if length pv /= depth then myThreadId >>= killThread >> yield else do
    let bestMove = head pv
    putStr $ "info depth " ++ show depth ++ " score " ++ (show score) ++ " pv "
    putStr . intercalate " " . map pp $ pv
    putStrLn ""
    void $ swapMVar mv (Just bestMove)

searchIterDeep :: [BestMoveParam] -> GameState -> IO ()
searchIterDeep params game = do
  mv <- newMVar Nothing
  tid <- forkIO $ mapM_ (searchIterDeep' mv game) [1..]
  threadDelay $ calcTimeToSearch (toMove game) params
  move <- takeMVar mv
  killThread tid
  putStr "bestmove "
  case move of
    Just m -> putStr . pp $ m
    Nothing -> putStrLn "(none)"
  putStrLn ""

applyMoves :: GameState -> [Move] -> Uci ()
applyMoves game moves = case foldM makeMove game moves of
  Just g -> put g
  Nothing -> lift $ putStrLn "Error making moves.  Possible illegal move."

handleCommand :: UCICommand -> Uci ()
handleCommand UCI = lift $ putStrLn $ "id name hchess\n" ++
                                      "id author Matthew Leach\n" ++
                                      "uciok"
handleCommand IsReady = lift $ putStrLn "readyok"
handleCommand Display = get >>= lift . putStrLn . pp
handleCommand Quit    = lift exitSuccess
handleCommand UCINewGame = do
  put newGame
  lift $ putStrLn ""
handleCommand (Position StartPos moves) = applyMoves newGame moves
handleCommand (Position (FENPos fen) moves) = do
  case processFen fen of
    Left e -> lift $ print e
    Right game -> applyMoves game moves

handleCommand (Go (BestMove params)) = get >>= lift . searchIterDeep params

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
