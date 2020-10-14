module Search (search) where

import Game
import Piece
import Move
import Data.Tree
import MoveGen
import Evaluate
import Data.List
import Data.Ord
import Control.Monad
import Data.Either.Extra

data SearchState = SearchState
  { colour :: Double
  , alpha :: Double
  , beta :: Double }
  deriving(Show)

data EvaluatedNode = EvaluatedNode
  { eval ::Double
  , state :: GameState }

inf :: Double
inf = read "Infinity"

unfoldGame :: GameState -> (GameState, [GameState])
unfoldGame game = (game, moveGen game)

prune :: Int -> Tree a -> Tree a
prune 0 (Node g _)  = Node g []
prune n (Node g sg) = Node g (map (prune $ n - 1) sg)

abDescend :: SearchState -> Tree GameState -> EvaluatedNode
abDescend s n@(Node g _) = EvaluatedNode v g
  where flipState = SearchState (-colour s) (-beta s) (-alpha s)
        v         = - (eval $ negaab flipState n)

updateState :: SearchState -> EvaluatedNode -> SearchState
updateState s n = SearchState (colour s) (max (eval n) $ alpha s) (beta s)

type ScanState = (SearchState, EvaluatedNode)

abScan :: ScanState -> Tree GameState -> Either ScanState ScanState
abScan (s, n) child = if alpha newState >= beta newState then Left y else Right y
  where newNode  = abDescend s child
        bestNode = maximumBy (comparing eval) [n, newNode]
        newState = updateState s bestNode
        y        = (newState, bestNode)

negaab :: SearchState -> Tree GameState -> EvaluatedNode
negaab s (Node g []) = EvaluatedNode (colour s * evaluate g) g
negaab s (Node _ (child:children)) = snd $ fromEither $ foldM abScan initialState children
  where firstNode    = abDescend s child
        initialState = (updateState s firstNode, firstNode)

search :: GameState -> Int -> Move
search g depth   = head . madeMoves $ state bestNode
  where s        = if toMove g == White then SearchState 1 (-inf) inf
                                        else SearchState (-1) inf (-inf)
        bestNode = negaab s . prune depth . unfoldTree unfoldGame $ g
