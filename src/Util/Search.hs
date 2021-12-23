module Util.Search where

import Control.Monad.State
    ( MonadState(get, put)
    , State
    , evalState
    , forM_
    , modify
    )
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import qualified Data.PSQueue as P

data DijkstraState a t =
    DijkstraState (P.PSQ a t) (M.Map a t) (M.Map a a)

minimumVertex :: (Ord a, Ord t) => P.PSQ a t -> a
minimumVertex = P.key . fromJust . P.findMin

handleNeighbor ::
       (Ord a, Ord t, Num t)
    => a -- ^Source Node
    -> (a -> a -> t) -- ^Cost function from source to neighbor
    -> a -- ^Neighbor
    -> DijkstraState a t -- ^Original state
    -> DijkstraState a t
handleNeighbor u costF v (DijkstraState q dist prev) = do
    let alt = dist M.! u + costF u v
    if alt < fromJust (P.lookup v q)
        then DijkstraState
                 (P.insert v alt q)
                 (M.insert v alt dist)
                 (M.insert v u prev)
        else DijkstraState q dist prev

dijkstraM ::
       (Ord a, Ord t, Num t)
    => (a -> Bool) -- ^Check if node is target
    -> (a -> [a]) -- ^Generate neighbors for a node
    -> (a -> a -> t) -- ^Cost function from node to other node
    -> (M.Map a t -> M.Map a a -> b) -- ^Generate answer based on the final scores and path
    -> State (DijkstraState a t) b
dijkstraM isTarget getNeighbors costF answerF = do
    (DijkstraState q dist prev) <- get
    if P.null q
        then return $ answerF dist prev
        else do
            let u = minimumVertex q
            let q' = P.delete u q
            put (DijkstraState q' dist prev)
            if isTarget u
                then return $ answerF dist prev
                else do
                    forM_
                        (filter (isJust . (`P.lookup` q)) (getNeighbors u))
                        (modify . handleNeighbor u costF)
                    dijkstraM isTarget getNeighbors costF answerF

dijkstra ::
       (Ord a, Ord t, Num t)
    => a
    -> a
    -> [a]
    -> (a -> [a])
    -> (a -> a -> t)
    -> (M.Map a t -> M.Map a a -> b)
    -> t
    -> b
dijkstra source target vs getNeighbors costF anserF maxT =
    evalState (dijkstraM isTarget getNeighbors costF anserF) s
  where
    isTarget = (==) target
    s =
        DijkstraState
            (P.insert source 0 $ P.fromList $ map (P.:-> maxT) vs)
            (M.singleton source 0)
            M.empty
