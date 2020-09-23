module Search (search) where

import Game
import Piece
import Move
import Data.Tree
import MoveGen
import Evaluate
import Data.Ord

data EvaluatedNode a = EvaluatedNode GameState a

instance (Eq a) => Eq (EvaluatedNode a) where
  (==) (EvaluatedNode _ x) (EvaluatedNode _ y) = x == y
 
instance (Ord a) => Ord (EvaluatedNode a) where
  compare (EvaluatedNode _ x) (EvaluatedNode _ y) = compare x y

evalNode :: GameState -> EvaluatedNode Double
evalNode g = EvaluatedNode g $ evaluate g

getState :: EvaluatedNode a -> GameState
getState (EvaluatedNode g _) = g

unfoldGame :: GameState -> (GameState, [GameState])
unfoldGame game = (game, moveGen game)

prune :: Int -> Tree a -> Tree a
prune 0 (Node g _)  = Node g []
prune n (Node g sg) = Node g (map (prune $ n - 1) sg)

minimize :: (Ord a) => Tree a -> a
minimize (Node a []) = a
minimize (Node a as) = minimum (map maximize as)

maximize :: (Ord a) => Tree a -> a
maximize (Node a []) = a
maximize (Node a as) = maximum (map minimize as)

search :: GameState -> Int -> Move
search g depth = head . madeMoves $ getState bestNode
  where minmax   = if toMove g == White then maximize else minimize
        bestNode = minmax . fmap evalNode . prune depth . unfoldTree unfoldGame $ g
