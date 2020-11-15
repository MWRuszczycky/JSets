{-# LANGUAGE OverloadedStrings #-}

module Model.Journals
    ( -- Working with journal sets
      pack
    , unpack
    , emptyJSets
    , lookupJSet
    , combineJSets
    , yearlySets
    , splitByFreq
    , issuesByAbbr
      -- Working with journal issues
    , addIssues
    , issueAtDate
    , issuesFromDate
    , issuesByDates
    , issuesInYear
    , issuesInVolume
    , lookupIssue
    , nextWeekly
    , nextMonthly

      -- Working with rank matching
    , score
    , match
      -- Working with downloaded table of contents
    , eUtilsUrl
    , eSearchUrl
    , eSummaryUrl
    , tocESearchQuery
    , tocESumQuery
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Dates     as D
import qualified Model.Core.Core      as C
import qualified Model.Core.Hungarian as Hn
import qualified Data.Text            as Tx
import qualified Data.Time            as Tm
import qualified Network.Wreq         as Wreq
import           Data.Time                    ( Day             )
import           Data.Text                    ( Text            )
import           Lens.Micro                   ( (.~), (&)       )
import           Data.List                    ( (\\), find
                                              , intersect       )

-- =============================================================== --
-- Working with journal sets

---------------------------------------------------------------------
-- Basic operations

pack :: [T.JSet a] -> T.JSets a
pack = T.JSets

unpack :: T.JSets a -> [T.JSet a]
unpack (T.JSets jsets) = jsets

emptyJSets :: T.JSets a
emptyJSets = T.JSets []

lookupJSet :: Int -> T.JSets a -> Maybe (T.JSet a)
lookupJSet k (T.JSets jsets) = find ( (==k) . T.setNo ) jsets

combineJSets :: T.MayMix a => [T.JSets a] -> T.JSets a
combineJSets = pack . T.stir . concatMap unpack

---------------------------------------------------------------------
-- Creation of yearly journal sets

yearlySets :: Int -> Int -> T.References -> T.JSets T.Issue
-- Compute the issues in each journal set for a specified year given
-- some frequency k (in weeks) of the journal sets.
yearlySets y k refs
    | y < 2000  = pack []
    | k < 1     = pack []
    | otherwise = let (ws,ms) = splitByFreq refs
                      wsets   = weeklyInYear y k ws
                      msets   = monthlyInYear y k ms
                      sets    = filter ( not . null ) $ C.zipLists wsets msets
                  in  pack [ T.JSet n i [] | (n, i) <- zip [1..] sets ]

setsInYear :: Int -> Int
setsInYear k
    | k < 1     = 0
    | r == 0    = q
    | otherwise = q + 1
    where (q,r) = quotRem 52 k

weeklyInYear :: Int -> Int -> T.References -> [[T.Issue]]
weeklyInYear y k = foldr go start . map (weeklyIssues y k)
    where start   = replicate (setsInYear k) []
          go x xs = C.zipLists x xs

weeklyIssues :: Int -> Int -> T.Issue -> [[T.Issue]]
weeklyIssues y k x = xs <> replicate ( setsInYear k - length xs ) []
    where xs = C.chunksOf k . issuesInYear y $ x

groupMonthly :: Int -> [a] -> [[a]]
-- Group the elements of a list of into n sublists so that they are
-- more-or-less evenly distributed. The elements are distributed to
-- push larger lists towards the end of the list. This helps to
-- ensure journal sets become available as soon as possible, because
-- monthly issues are guaranteed to be published only at the end of
-- each month. The length of the returned list is n or 0 if n < 1.
-- n   : site of the list to be created (i.e., the number of bins)
-- q   : min number monthly issues per set
-- q+1 : max number monthly issues per set, there will be r of these
-- v   : initial number of sets with only q issues each
-- u   : after the first v sets, we have u - 1 sets with only q
--       monthly issues than one set with q + 1 sets & this repeats.
groupMonthly n xs
    | n <  1    = []
    | null xs   = replicate n []
    | r == 0    = C.chunksOf q xs
    | q == 0    = replicate v [] <> ys
    | otherwise = C.chunksOf q vs <> ys
    where (q,r)       = quotRem (length xs) n
          (u,v)       = quotRem n r
          (vs,us)     = splitAt ( v * q ) xs
          (ys,_,_)    = iterate f ([],us,1) !! ( n - v )
          f (ps,zs,t) | rem t u == 0 = g $ q + 1
                      | otherwise    = g   q
                      where g m = let (ts,rs) = splitAt m zs
                                  in  (ps <> [ts], rs, t+1)

monthlyInYear :: Int -> Int -> T.References -> [[T.Issue]]
monthlyInYear y k = groupMonthly n . C.collate 1 . map (issuesInYear y)
    where n = setsInYear k

---------------------------------------------------------------------
-- Helper functions

issuesByAbbr :: T.HasIssue a => Text -> [a] -> [a]
-- ^Pull all issues in a list for a given journal abbr.
issuesByAbbr abbr = filter ( (== abbr) . T.abbr . T.journal )

splitByFreq :: T.HasIssue a => [a] -> ([a], [a])
splitByFreq = foldr go ([],[])
    where go x (ws,ms) = case T.freq . T.journal $ x of
                              T.Monthly -> (ws, x:ms)
                              _         -> (x:ws, ms)

-- =============================================================== --
-- Working with base journal issues

issueAtDate :: Day -> T.Issue -> T.Issue
-- ^Given an issue, find future issue current at the date provided.
-- If the future issue is published after the date provided, then it
-- is returned. Otherwise, the future issue that is expected to be
-- available closest to the provided date is returned. An issue is
-- expected to be available *after* its publication day (weekly) or
-- month (monthly). So, a weekly issue published on January 1, 2016
-- will not be current until January 2, 2016, and a monthly issue
-- published in June 2018 will not be current until July 2018.
issueAtDate d x = let go ~(y0:y1:ys) | T.theDate y1 >= d = y0
                                     | otherwise      = go (y1:ys)
                   in  case T.freq . T.theJournal $ x of
                            T.Monthly -> go . iterate nextMonthly $ x
                            _         -> go . iterate nextWeekly  $ x

issuesByDates :: Day -> Day -> T.Issue -> [T.Issue]
-- ^List of all issues available at the dates provided and in between.
-- The reference issue must be available at the first date.
issuesByDates d0 d1 ref
    | d1 > d1   = []
    | otherwise = takeWhile ( (<= d1) . T.theDate ) . issuesFromDate d0 $ ref

issuesFromDate :: Day -> T.Issue -> [T.Issue]
-- ^List of all issues from a given date on. The reference issue must
-- be available by the start date.
issuesFromDate d0 ref = iterate (addIssues 1) . issueAtDate d0 $ ref

issuesInYear :: Int -> T.Issue -> [T.Issue]
-- ^All issues with publication dates in the specified year.
issuesInYear y x = take 52 . filter ((== y) . T.year) $ xs
    where d0 = Tm.fromGregorian (fromIntegral   y    ) 1  2
          d1 = Tm.fromGregorian (fromIntegral $ y + 1) 1  1
          xs = issuesByDates d0 d1 x

issuesInVolume :: Int -> T.Issue -> [T.Issue]
issuesInVolume v = takeWhile ((==v) . T.theVolNo)
                   . dropWhile ((<v) . T.theVolNo)
                   . iterate (addIssues 1)

lookupIssue :: T.References -> Text -> (Int, Int) -> Maybe T.Issue
lookupIssue refs abbr (v,n) = find ( (== abbr) . T.abbr . T.theJournal ) refs
                              >>= find ( (== n) . T.theIssNo ) . issuesInVolume v

addIssues :: Int -> T.Issue -> T.Issue
addIssues n x
    | n < 0     = x
    | otherwise = let go f = (!! n) . iterate f $ x
                  in  case T.freq . T.theJournal $ x of
                           T.Monthly -> go nextMonthly
                           _         -> go nextWeekly

nextMonthly :: T.Issue -> T.Issue
nextMonthly x1
    | m2 == 1 && resets = x2 { T.theVolNo = v2, T.theIssNo = 1 }
    | m2 == 1           = x2 { T.theVolNo = v2 }
    | otherwise         = x2
    where (y2,m2,_) = Tm.toGregorian . Tm.addGregorianMonthsClip 1 . T.theDate $ x1
          d2        = Tm.fromGregorian y2 m2 (Tm.gregorianMonthLength y2 m2)
          v2        = succ . T.theVolNo $ x1
          n2        = succ . T.theIssNo $ x1
          x2        = x1 { T.theIssNo = n2, T.theDate = d2 }
          resets    = T.resets . T.theJournal $ x1

nextWeekly :: T.Issue -> T.Issue
nextWeekly x1
    | D.sameYear d1 d3 = x2 { T.theDate = d2 }
    | dropLast         = x2 { T.theDate = d3, T.theVolNo = v2, T.theIssNo = n2y }
    | D.sameYear d1 d2 = x2 { T.theDate = d2 }
    | everyWeek        = x2 { T.theDate = d2, T.theVolNo = v2, T.theIssNo = n2y }
    | otherwise        = x2 { T.theDate = d3, T.theVolNo = v2, T.theIssNo = n2y }
    where dropLast  = (T.freq . T.theJournal) x1 == T.WeeklyLast
          everyWeek = (T.freq . T.theJournal) x1 == T.Weekly
          d1  = T.theDate x1
          d2  = Tm.addDays 7 d1
          d3  = Tm.addDays 14 d1
          v2  = succ . T.theVolNo $ x1
          n2  = succ . T.theIssNo $ x1
          x2  = x1 { T.theIssNo = n2 }
          n2y = if T.resets . T.theJournal $ x1 then 1 else n2

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

-- =============================================================== --
-- Working with downloaded table of contents for journal issues
-- PMC queries of the PubMed data base are in two stages. The first
-- uses the ESearch E-utility to obtain the unique ID (UID) numbers
-- of the requested information (here each article citation).
-- These UIDs need to be submitted in a second request to PMC to
-- acquire the actual citations. This is done using the ESummary
-- E-utility. The following two functions are used to construct query
-- urls for use with ESearch and ESummary.
-- For more information see
-- https://www.ncbi.nlm.nih.gov/books/NBK25501
-- https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary
-- https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Searching_a_Database

eUtilsUrl :: String
eUtilsUrl = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"

eSearchUrl :: String
eSearchUrl = eUtilsUrl <> "esearch.fcgi?db=pubmed"

eSummaryUrl :: String
eSummaryUrl = eUtilsUrl <> "esummary.fcgi?db=pubmed"

eSearchTerm :: T.HasIssue a => a -> Text
eSearchTerm x = Tx.intercalate " AND " keys
    where keys = [ "\"" <> ( T.pubmed . T.journal) x <> "\"[journal]"
                 , ( C.tshow . T.year  ) x <> "[ppdat]"
                 , ( C.tshow . T.issNo ) x <> "[issue]"
                 , "journal article[pt]"
                 ]

tocESearchQuery :: T.HasIssue a => a -> Wreq.Options
-- ^Build an E-Utilities query to obtain the PMIDs for all citations
-- in the PubMed database associated with the given journal issue.
tocESearchQuery x = Wreq.defaults & Wreq.param "retmode" .~ [ "json"        ]
                                  & Wreq.param "retmax"  .~ [ "200"         ]
                                  & Wreq.param "term"    .~ [ eSearchTerm x ]

tocESumQuery :: [Text] -> Wreq.Options
-- ^Build an E-Utilities query to obtain the document summaries from
-- the PubMed database for the indicated PMIDs.
tocESumQuery pmids = let idstr = Tx.intercalate "," pmids
                     in  Wreq.defaults & Wreq.param "retmode" .~ [ "json" ]
                                       & Wreq.param "id"      .~ [ idstr  ]
