{-# LANGUAGE LambdaCase #-}

module Model.Core.Hungarian
    (
    ) where

import qualified Data.Map.Strict            as Map
import           Data.Map.Strict                    ( (!)          )
import           Data.List                          ( (\\)
                                                    , nub
                                                    , foldl'
                                                    , partition
                                                    , delete       )
import           Control.Monad.State.Strict         ( State (..)
                                                    , gets
                                                    , modify
                                                    , execState
                                                    , runState     )

-- =============================================================== --
-- Rudimentary implementation of the Hungarian algorithm
-- =============================================================== --

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

solve :: Weights -> Working
solve w = execState reset g
    where g = initWorking w

---------------------------------------------------------------------
-- Initialization

initWorking :: Weights -> Working
initWorking ws = let (xs,ys)    = initXYGraphs ws
                     swap (x,y) = (y,x)
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

---------------------------------------------------------------------
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
augment path = (<> toAdd') . filter ( not . flip elem toRem )
    where (toAdd, toRem) = splitOddEven path
          toAdd'         = [ (x,y) | (y,x) <- toAdd ]

equalitySubGraph :: Weights -> Labels -> Graph -> Graph
-- ^Restrict incident vertices to the equality subgraph.
equalitySubGraph ws ls vs = map ( \ (v,us) -> (v, filter (go v) us) ) vs
    where go v u = elem u names && score ws ls (v,u) == 0
          names  = map fst vs

---------------------------------------------------------------------
-- Basic graph operations

lookup' :: Int -> [(Int,Int)] -> Maybe Int
-- ^Reverse lookup function (i.e., match on the second element).
lookup' _ []          = Nothing
lookup' k ((x,y):xys)
    | y == k    = Just x
    | otherwise = lookup' k xys

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

splitOddEven :: [a] -> ([a],[a])
-- ^Split a list into even and odd number elements starting from 1.
splitOddEven xs = (snd . unzip $ os, snd . unzip $ es )
    where (os,es) = partition (odd . fst) . zip [1..] $ xs

allPairs :: [Int] -> [Int] -> [(Int,Int)]
-- ^Generate all pairs of each element of the first list with all
-- elements of the second list.
allPairs []     _    = []
allPairs _      []   = []
allPairs (x:xs) (ys) = allPairs xs ys <> [ (x,y) | y <- ys ]

score :: Weights -> Labels -> (Int, Int) -> Int
-- ^Standard scoring function for the Hungarian algorithm.
score ws ls (x,y) = ls ! x + ls ! y - ws ! (x,y)

-- =============================================================== --
-- Testing

type TestSet = ( String, Int, [((Int,Int),Int)] )

runTests :: IO ()
runTests = mapM_ runTest [test1, test2, test3, test4]

runTest :: TestSet -> IO ()
runTest (name, expected, weights) = do
    let ws = Map.fromList weights
        ms = matches . solve $ ws
        s  = sum . map ( \ m -> ws ! m ) $ ms
    if s == expected
       then putStrLn $ "Test " <> name <> " passed!"
       else do putStrLn $ "Test " <> name <> " failed!"
               putStrLn $ "   expected: " <> show expected <> ", got: " <> show s

test1 :: TestSet
-- solution is: 271
-- (1,7) (2,5) (3,6) (4,8)
test1 = ("1", 271, m)
    where m = [ ( (1,5), 42 ), ( (1,6), 53 ), ( (1,7), 53 ), ( (1,8),  7 )
              , ( (2,5), 94 ), ( (2,6), 70 ), ( (2,7), 52 ), ( (2,8), 21 )
              , ( (3,5), 78 ), ( (3,6), 82 ), ( (3,7), 47 ), ( (3,8), 72 )
              , ( (4,5), 31 ), ( (4,6),  2 ), ( (4,7), 43 ), ( (4,8), 42 )
              ]

test2 :: TestSet
test2 = ("2", 5, m)
    where m = [ ((1,3), 1), ((1,4), 2)
              , ((2,3), 3), ((2,4), 4)
              ]

test3 :: TestSet
-- solution is:
-- (1,5) (2,6) (3,4)
test3 = ("3", 16, m)
    where m = [ ((1,4), 1), ((1,5), 6), ((1,6), 0)
              , ((2,4), 0), ((2,5), 8), ((2,6), 6)
              , ((3,4), 4), ((3,5), 0), ((3,6), 1)
              ]

test4 :: TestSet
-- solution is: 271
-- (1,7) (2,5) (3,6) (4,8)
test4 = ("4", 437, m)
    where m = [ ( (1,7), 30 ), ( (1,8), 44 ), ( (1,9), 14 ), ( (1,10), 67 ), ( (1,11), 67 ), ( (1,12), 92 )
              , ( (2,7), 10 ), ( (2,8), 50 ), ( (2,9), 22 ), ( (2,10), 31 ), ( (2,11), 52 ), ( (2,12), 53 )
              , ( (3,7), 55 ), ( (3,8), 19 ), ( (3,9), 54 ), ( (3,10), 36 ), ( (3,11), 13 ), ( (3,12), 86 )
              , ( (4,7), 39 ), ( (4,8), 52 ), ( (4,9),  4 ), ( (4,10), 63 ), ( (4,11), 10 ), ( (4,12), 81 )
              , ( (5,7), 86 ), ( (5,8), 28 ), ( (5,9), 82 ), ( (5,10), 72 ), ( (5,11), 85 ), ( (5,12), 82 )
              , ( (6,7), 60 ), ( (6,8), 58 ), ( (6,9), 43 ), ( (6,10), 99 ), ( (6,11), 43 ), ( (6,12), 26 )
              ]
