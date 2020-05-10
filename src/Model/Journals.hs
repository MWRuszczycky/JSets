{-# LANGUAGE OverloadedStrings #-}

module Model.Journals
    ( -- Working with journal sets
      pack
    , unpack
    , emptyCollection
    , lookupJSet
    , selectNone
    , yearly26Sets
    , splitByFreq
    , dateOfJSet
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
      -- Working with selection sets
    , groupSelections
    , filterWithSelected
    , selectedCitations
      -- Working with downloaded table of contents
    , tocQuery
    ) where

import qualified Model.Core.Types        as T
import qualified Model.Core.Dates        as D
import qualified Model.Core.Core         as C
import qualified Data.Time               as Tm
import qualified Data.Map.Strict         as Map
import qualified Network.Wreq            as Wreq
import           Data.Time                          ( Day             )
import           Data.Text                          ( Text            )
import           Data.List                          ( find, nub, sort )
import           Lens.Micro                         ( (.~), (&)       )

-- =============================================================== --
-- Working with journal sets

---------------------------------------------------------------------
-- Basic operations

pack :: [T.JournalSet T.Issue] -> T.Collection
pack = Map.fromList . map go
    where go (T.JSet k xs) = (k, xs)

unpack :: T.Collection -> [T.JournalSet T.Issue]
unpack = map go . Map.toList
    where go (k, xs) = T.JSet k xs

emptyCollection :: T.Collection
emptyCollection = Map.empty

lookupJSet :: Int -> T.Collection -> Maybe (T.JournalSet T.Issue)
lookupJSet k col = T.JSet k <$> Map.lookup k col

selectNone :: T.JournalSet T.Issue -> T.JournalSet T.SelIssue
selectNone (T.JSet setNo xs) = T.JSet setNo . map (flip T.SelIssue []) $ xs

---------------------------------------------------------------------
-- Creation of yearly journal sets

yearly26Sets :: Int -> T.References -> T.Collection
---- ^Compute 26 journal sets that cover all issues published in a
---- given year. The first 24 sets account for all monthly issues and
---- the first 48 weekly issues. The 25-th set accounts for 49-th and
---- 50-th weekly issues. The 26-th set is all straglers that may or
---- may not be published in the specified year.
yearly26Sets y refs = pack . zipWith T.JSet [1..] $ C.zipLists wsets msets
    where (ws,ms) = splitByFreq refs
          wsets   = weekly26InYear y ws
          msets   = monthly26InYear y ms

weekly26InYear :: Int -> T.References -> [[T.Issue]]
-- ^Compute 26 sets of weekly issues. The first 24 sets contain two
-- issues from each journal. The first 25 sets contain 2 issues from
-- each journal. The 26-th set contains 1 or 2 issues from each set
-- depending on whether 51 or 52 issues are published that year.
weekly26InYear y = foldr go start . map (C.chunksOf 2 . issuesInYear y)
    where start     = replicate 26 []
          go xss ws = C.zipLists xss ws

monthly26InYear :: Int -> T.References -> [[T.Issue]]
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

dateOfJSet :: T.IsIssue a => T.JournalSet a -> Day
dateOfJSet = maximum . map T.date . T.issues

issuesByAbbr :: T.IsIssue a => Text -> [a] -> [a]
-- ^Pull all issues in a list for a given journal abbr.
issuesByAbbr abbr = filter ( (== abbr) . T.abbr . T.journal )

splitByFreq :: T.IsIssue a => [a] -> ([a], [a])
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
issuesInYear y x = take 52 . filter ((== y) . D.getYear . T.theDate) $ xs
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
-- Working with selection sets for review

groupSelections :: [T.JournalSet T.SelIssue] -> Maybe (T.JournalSet T.SelIssue)
-- ^Fold multiple selections into a single selection by combining
-- page numbers for the same issues between the selection sets.
groupSelections []       = Nothing
groupSelections ss@(x:_) = let setNo = T.setNo x
                           in  pure . T.JSet setNo
                                    . groupPages
                                    . concatMap T.issues $ ss

groupPages :: [T.SelIssue] -> [T.SelIssue]
groupPages = concatMap rewrap . C.collectBy go
    where go x y         = T.issue x == T.issue y
          rewrap []      = []
          rewrap (x:xs)  = [ T.SelIssue (T.issue x) . sort . nub
                             . concatMap T.selection $ (x:xs) ]

filterWithSelected :: [T.CitedIssue] -> [T.CitedIssue]
filterWithSelected = concatMap go
    where go iss = case selectedCitations iss of
                        [] -> []
                        cs -> [ iss { T.citations = cs } ]

selectedCitations :: T.CitedIssue -> [T.Citation]
selectedCitations (T.CitedIssue iss cs) = filter go cs
    where ps   = T.selection iss
          go c = elem (fst . T.pages $ c) ps

-- =============================================================== --
-- Working with downloaded table of contents for journal issues

tocQuery :: T.IsIssue a => a -> Wreq.Options
-- ^Build a table of contents query for PubMed for a given journal
-- issue. See https://www.ncbi.nlm.nih.gov/books/NBK3862/ for more
-- detail about how these queries are formed.
tocQuery x = let j = "\"" <> (T.pubmed . T.journal $ x) <> "\""
                 y = C.tshow . D.getYear . T.date $ x
                 n = C.tshow . T.issNo $ x
                 t = "journal article"
             in  Wreq.defaults & Wreq.param "dispmax" .~ ["100"]
                               & Wreq.param "format" .~ ["text"]
                               & Wreq.param "term" .~ [ j <> "[journal]"
                                                      , y <> "[ppdat]"
                                                      , n <> "[issue]"
                                                      , t <> "[pt]"
                                                      ]
