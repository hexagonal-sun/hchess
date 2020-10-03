module Search (search) where

import Game
import Piece
import Move
import Data.Tree
import MoveGen
import Evaluate

data ABNode a = EvaluatedNode GameState a | Inf | NegInf

instance (Eq a) => Eq (ABNode a) where
  (==) (EvaluatedNode _ x) (EvaluatedNode _ y) = x == y
  (==) _ _    = False

instance (Ord a) => Ord (ABNode a) where
  compare (EvaluatedNode _ x) (EvaluatedNode _ y) = compare x y
  compare Inf (EvaluatedNode _ _) = GT
  compare NegInf (EvaluatedNode _ _) = LT
  compare (EvaluatedNode _ _) Inf = LT
  compare (EvaluatedNode _ _) NegInf = GT
  compare Inf _ = error "Internal error: Cannot compare Inf with non-EvaluatedNode"
  compare NegInf _ = error "Interal error: Cannot compare NegInf with non-EvaluatedNode"

evalNode :: GameState -> ABNode Double
evalNode g = EvaluatedNode g $ evaluate g

getState :: ABNode a -> GameState
getState (EvaluatedNode g _) = g
getState _ = error "Internal error: cannot get state of Inf or NegInf node"

unfoldGame :: GameState -> (GameState, [GameState])
unfoldGame game = (game, moveGen game)

prune :: Int -> Tree a -> Tree a
prune 0 (Node g _)  = Node g []
prune n (Node g sg) = Node g (map (prune $ n - 1) sg)

minimize' :: (Ord a) => ABNode a -> ABNode a -> [Tree (ABNode a)] -> ABNode a
minimize' _ _ []                      = error "Internal error: minimize' on [] illegal"
minimize' alpha beta [x]              = maximize alpha beta x
minimize' alpha beta (n@(Node x _):xs) = if newBeta <= alpha then newNode else minimize' alpha newBeta xs
  where newNode = min x $ maximize alpha beta n
        newBeta = min beta newNode

minimize :: (Ord a) =>  ABNode a -> ABNode a -> Tree (ABNode a) -> ABNode a
minimize _ _ (Node a []) = a
minimize alpha beta (Node _ children) = minimize' alpha beta children

maximize' :: (Ord a) => ABNode a -> ABNode a -> [Tree (ABNode a)] -> ABNode a
maximize' _ _ []                      = error "Internal error: maximixe' on [] ilegal"
maximize' alpha beta [x]              = minimize alpha beta x
maximize' alpha beta (n@(Node x _):xs) = if newAlpha >= beta then newNode else maximize' newAlpha beta xs
  where newNode = max x $ maximize alpha beta n
        newAlpha = max alpha x

maximize :: (Ord a) =>  ABNode a -> ABNode a -> Tree (ABNode a) -> ABNode a
maximize _ _ (Node a []) = a
maximize alpha beta (Node _ children) = maximize' alpha beta children

search :: GameState -> Int -> Move
search g depth = head . madeMoves $ getState bestNode
  where minmax   = if toMove g == White then maximize else minimize
        bestNode = minmax NegInf Inf . fmap evalNode . prune depth . unfoldTree unfoldGame $ g
