module Util.Search where

import Control.Monad.State
    ( MonadState(get, put)
    , State
    , evalState
    , forM_
    , modify
    )
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.PSQueue as P
import qualified Data.Set as S

data DijkstraState a t =
    DijkstraState (S.Set a) (P.PSQ a t) (M.Map a t) (M.Map a a)

minimumVertex :: (Ord a, Ord t) => P.PSQ a t -> a
minimumVertex = P.key . fromJust . P.findMin

ltJust :: (Ord t, Num t) => t -> Maybe t -> Bool
ltJust _ Nothing = True
ltJust alt (Just c) = alt < c

handleNeighbor ::
       (Ord a, Ord t, Num t)
    => a -- ^Source Node
    -> (t, a) -- ^Neighbor and cost
    -> DijkstraState a t -- ^Original state
    -> DijkstraState a t
handleNeighbor u (cost, v) (DijkstraState s q dist prev) = do
    let alt = dist M.! u + cost
    if alt `ltJust` P.lookup v q
        then DijkstraState
                 (S.delete v s)
                 (P.insert v alt q)
                 (M.insert v alt dist)
                 (M.insert v u prev)
        else DijkstraState s q dist prev

dijkstraM ::
       (Ord a, Ord t, Num t)
    => (a -> Bool) -- ^Check if node is target
    -> (a -> [(t, a)]) -- ^Generate neighbors for a node and the cost
    -> (M.Map a t -> M.Map a a -> b) -- ^Generate answer based on the final scores and path
    -> State (DijkstraState a t) b
dijkstraM isTarget getNeighbors answerF = do
    (DijkstraState s q dist prev) <- get
    if P.null q
        then return $ answerF dist prev
        else do
            let u = minimumVertex q
            let q' = P.delete u q
            let s' = S.insert u s
            put (DijkstraState s' q' dist prev)
            if isTarget u
                then return $ answerF dist prev
                else do
                    forM_
                        (filter (not . (`S.member` s') . snd) (getNeighbors u))
                        (modify . handleNeighbor u)
                    dijkstraM isTarget getNeighbors answerF

dijkstra ::
       (Ord a, Ord t, Num t)
    => a -- ^Source node
    -> a -- ^Target node
    -> (a -> [(t, a)]) -- ^Generate neighbors and costs
    -> (M.Map a t -> M.Map a a -> b) -- ^Generate answer based on the final scores and path
    -> b
dijkstra source target getNeighbors answerF =
    evalState (dijkstraM isTarget getNeighbors answerF) s
  where
    isTarget = (==) target
    s =
        DijkstraState
            S.empty
            (P.singleton source 0)
            (M.singleton source 0)
            M.empty
