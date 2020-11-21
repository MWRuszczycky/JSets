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
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Dates     as D
import qualified Model.Core.Core      as C
import qualified Data.Time            as Tm
import           Data.Time                  ( Day  )
import           Data.Text                  ( Text )
import           Data.List                  ( find )

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
