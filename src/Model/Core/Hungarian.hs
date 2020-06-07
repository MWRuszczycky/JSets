{-# LANGUAGE LambdaCase #-}

module Model.Core.Hungarian
    ( solveMax
    , solveMin
    ) where

import qualified Data.Map.Strict            as Map
import           Data.Map.Strict                    ( (!)          )
import           Data.List                          ( (\\)
                                                    , nub
                                                    , sort
                                                    , foldl'
                                                    , intersect
                                                    , partition
                                                    , delete       )
import           Control.Monad.State.Strict         ( State
                                                    , gets
                                                    , modify
                                                    , execState    )

-- =============================================================== --
-- Rudimentary implementation of the Hungarian algorithm
-- Probably not the most beautiful implementation, but seems to work.

-- =============================================================== --
-- Local types

type Weights   = Map.Map (Int, Int) Int
type Labels    = Map.Map Int Int
type Vertex    = (Int, [Int])
type Graph     = [Vertex]
type Hungarian = State Working

data Working = Working {
      weights :: Weights
    , labels  :: Labels
    , matches :: [(Int,Int)]
    , allXs   :: Graph
    , allYs   :: Graph
    , subXs   :: Graph
    , subYs   :: Graph
    } deriving ( Show )

-- =============================================================== --
-- Interface

solveMax :: [((Int,Int),Int)] -> Either String (Int, [(Int,Int)])
solveMax edgeWeights = do
    start <- validate edgeWeights >>= pure . initWorking
    let result = execState reset start
        ws     = weights result
        ms     = sort . matches $ result
    pure ( sum . map (ws !) $ ms, ms )

solveMin :: [((Int,Int),Int)] -> Either String (Int, [(Int,Int)])
solveMin edgeWeights = do
    let negEdgeWeights = [ (k, negate v) | (k,v) <- edgeWeights ]
    (s, ms) <- solveMax negEdgeWeights
    pure (negate s, ms)

-- =============================================================== --
-- Initialization and validity checking

validate :: [((Int,Int),Int)] -> Either String Weights
validate edgeWeights
    | not isBipartite = Left "Graph is not bipartite."
    | not isBalanced  = Left "Disjoint subsets are not equal size."
    | not isComplete  = Left "Bipartite graph is not complete."
    | otherwise       = pure . Map.fromList $ edgeWeights
    where xys         = fst . unzip $ edgeWeights
          (xs,ys)     = let (xs', ys') = unzip xys in ( nub xs', nub ys' )
          isBipartite = null . intersect xs $ ys
          isBalanced  = length xs == length ys
          isComplete  = all (flip elem xys) . allPairs xs $ ys

initWorking :: Weights -> Working
initWorking ws = let (xs,ys) = initXYGraphs ws
                 in  Working { weights = Map.union ws . Map.mapKeys swap $ ws
                             , labels  = initLabels ws xs ys
                             , matches = []
                             , allXs   = xs
                             , allYs   = ys
                             , subXs   = []
                             , subYs   = []
                             }

initXYGraphs :: Weights -> (Graph,Graph)
-- ^Initialize the X and Y graphs from the weights.
initXYGraphs ws = ( graphFromEdges xys, graphFromEdges . map swap $ xys )
    where xys        = Map.keys ws
          swap (x,y) = (y,x)

initLabels :: Weights -> Graph -> Graph -> Labels
-- ^Initialize the labels. The trivial feasible labeling is used. So,
-- all x-vertices get their maximum edge weight and all y-vertices
-- get labeled with just 0.
initLabels ws xs ys = Map.fromList $ yLabels <> xLabels
    where yLabels = [ (y,0)         | (y, _) <- ys ]
          xLabels = [ (v, go v us)  | (v,us) <- xs ]
          go v us = maximum [ ws ! (v,u) | u <- us ]

-- =============================================================== --
-- Core algorithm

reset :: Hungarian ()
-- ^Check if all the x-vertices are matched. Reset X' to a free x
-- vertex, set Y' to empty and start another round of matching.
reset = do
    xs <- gets allXs
    mx <- gets $ map fst . matches
    -- Get all unmatched x-vertices. If none, then we're done.
    case filter (not . flip elem mx . fst ) xs of
         []  -> pure ()
         x:_ -> do modify $ \ s -> s { subXs = [x], subYs = [] }
                   grow

grow :: Hungarian ()
-- ^Look for another candidate y-vertex to add. If found add it,
-- otherwise we need to relabel the vertices and try again.
grow = nextY >>= \case
           Nothing -> relabel
           Just y  -> addNewY y

addNewY :: Vertex -> Hungarian ()
-- ^Add a new y-vertex to Y'. If y is matched, we also have to add
-- its partner to X'. If y is free, we have to augment.
addNewY (y,xs) = gets ( lookup' y . matches ) >>= \case
                     Just x  -> addMatch x y
                     Nothing -> addAugment (y,xs)

relabel :: Hungarian ()
-- ^Standard relabeling scheme. Afterwards, try to add a y-vertex.
relabel = do
    d  <- findDelta
    xs <- gets $ map fst . subXs
    ys <- gets $ map fst . subYs
    ls <- gets labels
    let lsx  = foldl' ( flip $ Map.adjust (subtract d) ) ls  xs
        lsxy = foldl' ( flip $ Map.adjust (+d        ) ) lsx ys
    modify $ \ s -> s { labels = lsxy }
    grow

findDelta :: Hungarian Int
-- ^Find the adjustment for the standard relabeling scheme.
-- delta = minumum_(x in X', y not in Nf(X'))( score x y )
-- score x y = label(x) + label(y) - weight(x,y)
-- Note that Y' == Nf(X') is assumed (i.e., nextY == Nothing).
findDelta = do
    ns <- neighbors
    ys <- gets $ (\\ ns) . map fst . allYs
    xs <- gets $ map fst . subXs
    ws <- gets weights
    ls <- gets labels
    pure . minimum . map (score ws ls) . allPairs xs $ ys

---------------------------------------------------------------------
-- Adding y-vertices. There are two ways to do depending on whether
-- the newly added y-vertex is free or already matched.

addAugment :: Vertex -> Hungarian ()
-- ^Add free y to Y' and augment the matching. After augmenting we
-- always reset the working state to allow another fre x-vertex to be
-- selected and keep the first selected x-vertex free. This way we
-- ensure an augmenting path when the next free y-vertex is added.
addAugment y = do
    ys   <- gets subYs
    modify $ \ s -> s { subYs = y:ys }
    path <- augmentingPath
    ms   <- gets matches
    modify $ \ s -> s { matches = augment path ms }
    reset

addMatch :: Int -> Int -> Hungarian ()
-- ^Add matched y and its partner to Y' and X'. Note that x and y are
-- already matched, so they are not added to the match list. We do
-- not reset at this point, because the first x-vertex is still free.
-- Instead we just try to grow the Y' and X' sets again.
addMatch x y = do
    yvs <- gets subYs
    xvs <- gets subXs
    yxs <- gets $ maybe [] id . lookup y . allYs
    xys <- gets $ maybe [] id . lookup x . allXs
    modify $ \ s -> s { subYs = (y,yxs) : yvs, subXs = (x,xys) : xvs }
    grow

---------------------------------------------------------------------
-- Finding y-vertices to add to Y' during each round of growing.

neighbors :: Hungarian [Int]
-- ^Find all y-vertices that are reachable via edges in the equality
-- graph from x-vertices in X'. This is usually denoted Nf(X'), where
-- f is the feasible labeling function.
neighbors = do
    ws <- gets weights
    ls <- gets labels
    let go (x,ys) = filter ( \ y -> 0 == score ws ls (x,y) ) ys
    gets $ nub . concatMap go . subXs

nextY :: Hungarian (Maybe Vertex)
-- ^Look for another y in Nf(X') - Y' to add to Y' unless none is
-- available. This is the checkpoint for Nf(X') == Y'.
nextY = do
    nxs <- neighbors
    yvs <- gets subYs
    ys  <- gets allYs
    case nxs \\ map fst yvs of
         []    -> pure Nothing
         (y:_) -> pure $ (,) y <$> lookup y ys

---------------------------------------------------------------------
-- Match augmentation.
-- When we add a free y-vertex to Y', we are
-- guaranteed an augmenting path in (X' U Y'), we need to find this
-- augmenting path and use it to augment the matching.
-- The augmenting path must be restricted to (X' U Y') and the
-- equality of the graph for the current labeling.

augmentingPath :: Hungarian [(Int,Int)]
-- ^Find the augmenting path from the last y-vertex added to Y' and
-- the first x-vertex added to X'. So the order of contenating the
-- X' and Y' subsets prior to restriction is critical.
augmentingPath = do
    ws <- gets weights
    ls <- gets labels
    xs <- gets subXs
    ys <- gets subYs
    let eg = equalitySubGraph ws ls $ ys <> xs
    pure $ findPath eg (last eg) (head eg)

augment :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
-- ^Given a path of an odd number of edges that ends on the first
-- free x-vertex to be added to the X' subset (i.e., xn),
--     [ 1 (y1,x1), 2 (x1,y2), 3 (y2,x3), .., n (yn,xn) ],
-- and the current matches, augment the path by removing the evenly
-- numbered edges and adding the odd-numbered edges.
augment path = (<> map swap toAdd) . filter ( not . flip elem toRem )
    where (toAdd, toRem) = splitOddEven path

equalitySubGraph :: Weights -> Labels -> Graph -> Graph
-- ^Restrict incident vertices to the equality subgraph.
equalitySubGraph ws ls vs = map ( \ (v,us) -> (v, filter (go v) us) ) vs
    where go v u = elem u names && score ws ls (v,u) == 0
          names  = map fst vs

---------------------------------------------------------------------
-- Basic graph operations and helper functions

lookup' :: Int -> [(Int,Int)] -> Maybe Int
-- ^Reverse lookup function (i.e., match on the second element).
lookup' _ []          = Nothing
lookup' k ((x,y):xys)
    | y == k    = Just x
    | otherwise = lookup' k xys

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

graphFromEdges :: [(Int,Int)] -> Graph
-- ^Generate a graph from edges based on the first edge only.
graphFromEdges []         = []
graphFromEdges ((x,y):es) = (x, snd . unzip $ xs) : graphFromEdges ys
    where (xs,ys) = partition ( (==x) . fst ) $ (x,y):es

findPath :: [Vertex] -> Vertex -> Vertex -> [(Int,Int)]
-- ^Find a path through a graph g from (y,xs) to stop.
findPath g stop@(x,_) (y,xs)
    | y == x    = []
    | elem x xs = [(y,x)]
    | otherwise = let g'  = map ( \ (v,us) -> (v, delete y us) ) g
                      nxt = filter ( flip elem xs . fst ) g'
                  in  case dropWhile null . map (findPath g' stop) $ nxt of
                           []             -> []
                           ((p,q):rest):_ -> (y,p):(p,q):rest
                           _              -> [] -- should be impossible

splitOddEven :: [a] -> ([a],[a])
-- ^Split a list into even and odd number elements starting from 1.
splitOddEven xs = (snd . unzip $ os, snd . unzip $ es )
    where (os,es) = partition (odd . fst) . zip [1..] $ xs

allPairs :: [Int] -> [Int] -> [(Int,Int)]
-- ^Generate all pairs of each element of the first list with all
-- elements of the second list.
allPairs []     _  = []
allPairs _      [] = []
allPairs (x:xs) ys = allPairs xs ys <> [ (x,y) | y <- ys ]

score :: Weights -> Labels -> (Int, Int) -> Int
-- ^Standard scoring function for the Hungarian algorithm.
score ws ls (x,y) = ls ! x + ls ! y - ws ! (x,y)
