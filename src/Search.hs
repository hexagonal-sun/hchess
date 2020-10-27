module Search (search, EvaluatedNode(..)) where

import Game
import Piece
import Data.Tree
import MoveGen
import Move
import Evaluate
import Data.List
import Data.Ord
import Control.Monad
import Data.Either.Extra

data SearchState = SearchState
  { alpha :: !Double
  , beta ::  !Double }

data EvaluatedNode = EvaluatedNode
  { eval ::  !Double
  , state :: !GameState }

inf :: Double
inf = read "Infinity"

unfoldGame :: GameState -> (GameState, [GameState])
unfoldGame game = (game, moveGen game)

prune :: Int -> Tree a -> Tree a
prune 0 (Node g _)  = Node g []
prune n (Node g sg) = Node g (map (prune $ n - 1) sg)

abDescend :: SearchState -> Tree GameState -> EvaluatedNode
abDescend s n = EvaluatedNode (-eval nextNode) (state nextNode)
  where flipState = SearchState (negate . beta $ s) (negate . alpha $ s)
        nextNode = negaab flipState n

updateState :: SearchState -> Double -> SearchState
updateState s score = SearchState (max score $ alpha s) (beta s)

type ScanState = (SearchState, EvaluatedNode)

selectBestNode :: (a -> a -> Ordering) -> a -> a -> a
selectBestNode cmp a b = if cmp a b == EQ then a else maxNode
  where maxNode = maximumBy cmp [a, b]

abScan :: ScanState -> Tree GameState -> Either ScanState ScanState
abScan  (s, n) child = if alpha newState >= beta newState then Left y else Right y
  where newNode  = abDescend s child
        bestNode = selectBestNode (comparing eval) n newNode
        newState = updateState s $ eval bestNode
        y        = (newState, bestNode)

negaab :: SearchState -> Tree GameState -> EvaluatedNode
negaab _ (Node g []) = EvaluatedNode ((if toMove g == White then id else negate) . evaluate $ g) g
negaab s (Node _ (child:children)) = snd $ fromEither $ foldM abScan initialState children
  where firstNode    = abDescend s child
        initialState = (updateState s $ eval firstNode, firstNode)

getPV :: GameState -> GameState -> [Move]
getPV g bestNode = moveList bestNode \\ moveList g
  where moveList = reverse . madeMoves

search :: GameState -> Int -> (Double, [Move])
search g depth = (evaluate . state $ bestNode, getPV g $ state bestNode)
  where s = SearchState (-inf) inf
        bestNode = negaab s . prune depth . unfoldTree unfoldGame $ g
