{-# LANGUAGE OverloadedStrings #-}

module Model.Matching
    ( score
    , match
    ) where

import qualified Model.Core.Hungarian as Hn
import qualified Model.Core.Types     as T
import           Data.List                  ( (\\), intersect )
import           Data.Text                  ( Text            )

-- =============================================================== --
-- Working with rank-matchings for assigning selections

match :: Text -> [Int] -> [(Text ,[[Int]])] -> T.MatchResult
-- ^Given a title, a set of paper indices (all positive) and a set of
-- rank lists (i.e., persons receiving papers), assign the papers to
-- person/rank list using the Hungarian algorithm. The number of
-- papers must be a multiple of the number of persons in the match.
-- This is accomplished by adding additonal 'phantom' papers with
-- negative indices. The match scores the phantoms most highly for
-- each person/rank list and includes them in the matching process.
-- Note, this matching can give one person more than one phantom.
match title []     _         =
    T.MatchResult title [] [] [] $ Left "Nothing to match."
match title papers ranklists =
    let (ks,rs)   = unzip ranklists
        nr        = length ranklists
        np        = length papers
        phantoms  = getPhantoms np nr
        nIDs      = quot (np + length phantoms) nr
        cIDs      = assignIDs (maximum papers + 1) . take nr . repeat $ nIDs
        scoreSets = map (scoreWithPhantoms phantoms papers) rs
        scores    = concat . zipWith assignScores scoreSets $ cIDs
    in  T.MatchResult { T.matchTitle  = title
                      , T.matchPapers = phantoms <> papers
                      , T.matchIDs    = zip ks cIDs
                      , T.matchScores = scores
                      , T.matchResult = Hn.solveMax scores
                      }

getPhantoms :: Int -> Int -> [Int]
-- ^Given the number of papers and rank lists, determine how many
-- phantom papers are needed to ensure that the total number of
-- papers is a multiple of the number of ranklists. Then create
-- indices for each phantom. These indices are negative to ensure
-- that they cannot overlap with real papers that are not indexed for
-- the current match.
getPhantoms nPapers nRankLists = map negate [1 .. n]
    where r = rem nPapers nRankLists
          n = if r == 0 then 0 else nRankLists - r

---------------------------------------------------------------------
-- Assignments

assignIDs :: Int -> [Int] -> [[Int]]
assignIDs _     []     = []
assignIDs start (n:ns) = ids : assignIDs (start + n) ns
    where ids = [ start .. start + n - 1 ]

assignScores :: [(Int,Int)] -> [Int] -> [((Int,Int), Int)]
assignScores scores = concatMap go
    where go cardID = [ ( (index, cardID), s ) | (index, s) <- scores ]

---------------------------------------------------------------------
-- Converting rank lists into preference scores

score :: [Int] -> [[Int]] -> [(Int, Int)]
-- ^Given a list of indices to include (with no repeats), and a list
-- of rankings (with no repeats), score the indices as follows:
-- 1.  Return the value as (index, score)
-- 2.  The maximum score is the number of indices.
-- 3.  The minimum score is 1.
-- 4.  If there are no indices, the result is an empty list.
-- 5.  Only indexed rankings are to be included in the scoring.
-- 6.  Any unranked indices all get the same score.
-- 7.  The ranking groups are divided in half with a higher ranked
--     'favored' half and the lower ranked 'disfavored' half. If there
--     is an odd number of ranking groups, the favored half has an
--     odd number of ranking groups.
-- 8.  All rankings in the same group get the same score.
-- 9.  The favored rankings are score from the maximum down.
-- 10  The unfavored rankings are scored from 1 up.
-- 11. Any unranked indices are ranked between the favored and the
--     unfavored towards the minimum score.
score indices vs  = concat $ scorex <> reverse scorey
    where n       = length indices
          vs'     = filter (not . null) [ intersect v indices | v <- vs ]
          missing = indices \\ concat vs'
          (xs,ys) = splitAt ( quot (1 + length vs') 2 ) vs'
          go zs k = zip zs (repeat k)
          scorex  = zipWith go xs [n, n-1 .. ]
          scorey  = zipWith go (reverse ys <> [missing]) $ [1 .. ]

scoreWithPhantoms :: [Int] -> [Int] -> [[Int]] -> [(Int,Int)]
-- ^Same as score, but give phantoms the maximum score equal to the
-- number of indices plus one.
scoreWithPhantoms phantoms indices vs = ps <> score indices vs
    where ps = zip phantoms . repeat $ length indices + 1
