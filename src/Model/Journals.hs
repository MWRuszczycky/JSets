{-# LANGUAGE OverloadedStrings #-}

module Model.Journals
    ( -- Working with journal sets
      yearly26Sets
    , splitByFreq
    , dateOfJSet
    , issuesByKey
      -- Working with journal issues
    , addIssues
    , issueAtDate
    , issuesFromDate
    , issuesByDates
    , issuesInYear
    , lookupIssue
    , nextWeekly
    , nextMonthly
    ) where

import qualified Model.Types        as T
import qualified Model.Dates        as D
import qualified Model.Core         as C
import qualified Model.References   as R
import qualified Data.Text          as Tx
import qualified Data.Time          as Tm
import qualified Data.Map.Strict    as Map
import           Data.Time                 ( Day  )
import           Data.Text                 ( Text )
import           Data.List                 ( find )

-- =============================================================== --
-- Working with journal sets

---------------------------------------------------------------------
-- Creation of yearly journal sets

yearly26Sets :: Int -> [T.Issue] -> T.JournalSets
---- ^Compute 26 journal sets that cover all issues published in a
---- given year. The first 24 sets account for all monthly issues and
---- the first 48 weekly issues. The 25-th set accounts for 49-th and
---- 50-th weekly issues. The 26-th set is all straglers that may or
---- may not be published in the specified year.
yearly26Sets y refs = Map.fromList . zip keys $ C.shuffleTogether wsets msets
    where (ws,ms) = splitByFreq refs
          wsets   = weekly26InYear y ws
          msets   = monthly26InYear y ms
          keys    = map ( \ n -> C.txt y <> "-" <> C.txt n ) [1..]

weekly26InYear :: Int -> [T.Issue] -> [[T.Issue]]
-- ^Compute 26 sets of weekly issues. The first 24 sets contain two
-- issues from each journal. The first 25 sets contain 2 issues from
-- each journal. The 26-th set contains 1 or 2 issues from each set
-- depending on whether 51 or 52 issues are published that year.
weekly26InYear y = foldr go start . map (C.chunksOf 2 . issuesInYear y)
    where start     = replicate 26 []
          go xss ws = C.shuffleTogether xss ws

monthly26InYear :: Int -> [T.Issue] -> [[T.Issue]]
-- ^Compute 26 sets of monthly issues. The first two sets contain no
-- issues at all. The remaining 24 sets contain either one or two
-- issues from each journal.
monthly26InYear y refs = [] : [] : C.shuffleIn byqs byqrs
    where ms    = C.collate 1 . map (issuesInYear y) $ refs
          (q,r) = quotRem (length refs) 2
          byqs  = C.takeEveryAt q (q+r) ms
          byqrs = C.takeEveryAt (q+r) q . drop q $ ms

---------------------------------------------------------------------
-- Helper functions

dateOfJSet :: T.JournalSet -> Day
dateOfJSet = maximum . map T.date . snd

issuesByKey :: Text -> [T.Issue] -> [T.Issue]
-- ^Pull all issues in a list for a given journal key.
issuesByKey key = filter ( (== key) . T.key . T.journal )

splitByFreq :: [T.Issue] -> ([T.Issue], [T.Issue])
splitByFreq = foldr go ([],[])
    where go x (ws,ms) = case T.freq . T.journal $ x of
                              T.Monthly -> (ws, x:ms)
                              _         -> (x:ws, ms)

-- =============================================================== --
-- Working with journal issues

issueAtDate :: Day -> T.Issue -> T.Issue
-- ^Given an issue, find future issue current at the date provided.
-- If the future issue is published after the date provided, then it
-- is returned. Otherwise, the future issue that is expected to be
-- available closest to the provided date is returned. An issue is
-- expected to be available *after* its publication day (weekly) or
-- month (monthly). So, a weekly issue published on January 1, 2016
-- will not be current until January 2, 2016, and a monthly issue
-- published in June 2018 will not be current until July 2018.
issueAtDate d x = let go ~(y0:y1:ys) | T.date y1 >= d = y0
                                      | otherwise      = go (y1:ys)
                   in  case T.freq . T.journal $ x of
                            T.Monthly -> go . iterate nextMonthly $ x
                            _         -> go . iterate nextWeekly  $ x

issuesByDates :: Day -> Day -> T.Issue -> [T.Issue]
-- ^List of all issues available at the dates provided and in between.
-- The reference issue must be available at the first date.
issuesByDates d0 d1 ref
    | d1 > d1   = []
    | otherwise = takeWhile ( (<= d1) . T.date ) . issuesFromDate d0 $ ref

issuesFromDate :: Day -> T.Issue -> [T.Issue]
-- ^List of all issues from a given date on. The reference issue must
-- be available by the start date.
issuesFromDate d0 ref = iterate (addIssues 1) . issueAtDate d0 $ ref

issuesInYear :: Int -> T.Issue -> [T.Issue]
-- ^All issues with publication dates in the specified year.
issuesInYear y x = take 52 . filter ((== y) . D.getYear . T.date) $ xs
    where d0 = Tm.fromGregorian (fromIntegral   y    ) 1  2
          d1 = Tm.fromGregorian (fromIntegral $ y + 1) 1  1
          xs = issuesByDates d0 d1 x

lookupIssue :: Text -> (Int, Int) -> Maybe T.Issue
-- ^Look up a journal issue by its journal key, volume and number.
lookupIssue jKey (vNo,nNo) = find ((== jKey) . T.key . T.journal) R.issueRefs
                             >>= find ((== nNo) . T.issNo)
                                 . takeWhile ((== vNo) . T.volNo)
                                 . dropWhile ((< vNo) . T.volNo)
                                 . iterate (addIssues 1)

addIssues :: Int -> T.Issue -> T.Issue
addIssues n x
    | n < 0     = x
    | otherwise = let go f = (!! n) . iterate f $ x
                  in  case T.freq . T.journal $ x of
                           T.Monthly -> go nextMonthly
                           _         -> go nextWeekly

nextMonthly :: T.Issue -> T.Issue
nextMonthly x1
    | m2 == 1 && resets = x2 { T.volNo = v2, T.issNo = 1 }
    | m2 == 1           = x2 { T.volNo = v2 }
    | otherwise         = x2
    where (y2,m2,_) = Tm.toGregorian . Tm.addGregorianMonthsClip 1 . T.date $ x1
          d2        = Tm.fromGregorian y2 m2 (Tm.gregorianMonthLength y2 m2)
          r2        = succ . T.refNo $ x1
          v2        = succ . T.volNo $ x1
          n2        = succ . T.issNo $ x1
          x2        = x1 { T.refNo = r2, T.issNo = n2, T.date = d2 }
          resets    = T.resets . T.journal $ x1

nextWeekly :: T.Issue -> T.Issue
nextWeekly x1
    | D.sameYear d1 d3 = x2 { T.date = d2 }
    | dropLast         = x2 { T.date = d3, T.volNo = v2, T.issNo = n2y }
    | D.sameYear d1 d2 = x2 { T.date = d2 }
    | everyWeek        = x2 { T.date = d2, T.volNo = v2, T.issNo = n2y }
    | otherwise        = x2 { T.date = d3, T.volNo = v2, T.issNo = n2y }
    where dropLast  = (T.freq . T.journal) x1 == T.WeeklyLast
          everyWeek = (T.freq . T.journal) x1 == T.Weekly
          d1  = T.date x1
          d2  = Tm.addDays 7 d1
          d3  = Tm.addDays 14 d1
          r2  = succ . T.refNo $ x1
          v2  = succ . T.volNo $ x1
          n2  = succ . T.issNo $ x1
          x2  = x1 { T.refNo = r2, T.issNo = n2 }
          n2y = if T.resets . T.journal $ x1 then 1 else n2
