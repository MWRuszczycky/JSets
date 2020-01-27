{-# LANGUAGE OverloadedStrings #-}

module Model.Journals
    ( -- Working with journal sets
      journalSets
    , splitByFreq
    , shuffleSets
      -- Working with journal issues
    , addIssues
    , issueAtDate
    , issuesFromDate
    , issuesByDates
    , issuesInYear
    , nextWeekly
    , nextMonthly
    ) where

import qualified Model.Types     as T
import qualified Model.Dates     as D
import qualified Model.Core      as C
import qualified Data.Time       as Tm
import           Data.Time              ( Day )

-- =============================================================== --
-- Working with journal sets

journalSets :: [[T.Issue]] -> [[T.Issue]] -> [[T.Issue]]
journalSets ws ms = [ w ++ m | (w, m) <- zip sws sms ]
    where sws   = C.chunksOf (2 * length ws) . shuffleSets 2 $ ws
          sms0  = shuffleSets 1 ms
          sms1  = C.takeEveryAt q (q+r) sms0
          sms2  = C.takeEveryAt (q+r) q . drop q $ sms0
          sms   = C.shuffleIn sms1 sms2
          (q,r) = quotRem (length ms) 2

shuffleSets :: Int -> [[T.Issue]] -> [T.Issue]
shuffleSets n = snd . foldr go (0,[])
    where go x (k,xs) = (k+n, C.shuffleInAt k n xs x)

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
-- ^All issues available by January 1 of the year to December 31.
-- Note that the issues published at the very end of the previous
-- year will be included as those available at January 1.
issuesInYear y = issuesByDates d0 d1
    where d0 = Tm.fromGregorian (fromIntegral y)  1  1
          d1 = Tm.fromGregorian (fromIntegral y) 12 31

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
