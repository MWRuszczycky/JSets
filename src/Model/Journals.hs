{-# LANGUAGE OverloadedStrings #-}

module Model.Journals
    ( -- Working with journal sets
      pack
    , unpack
    , emptyJSets
    , lookupJSet
    , combineJSets
    , yearly26Sets
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
      -- Working with selection sets
    , selectNone
    , emptyContent
    , restrictContent
    , missingPMIDs
    , addContent
    , addCitations
      -- Working with downloaded table of contents
    , eUtilsUrl
    , eSearchUrl
    , eSummaryUrl
    , tocESearchQuery
    , tocESumQuery
    ) where

import qualified Model.Core.Types as T
import qualified Model.Core.Dates as D
import qualified Model.Core.Core  as C
import qualified Data.Text        as Tx
import qualified Data.Time        as Tm
import qualified Network.Wreq     as Wreq
import           Data.Bifunctor           ( bimap      )
import           Data.Time                ( Day        )
import           Data.Text                ( Text       )
import           Lens.Micro               ( (.~), (&)  )
import           Data.List                ( (\\), find )

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

lookupJSet :: T.HasIssue a => Int -> T.JSets a -> Maybe (T.JSet a)
lookupJSet k (T.JSets jsets) = find ( (==k) . T.setNo ) jsets

combineJSets :: T.MayMix a => [T.JSets a] -> T.JSets a
combineJSets = pack . T.stir . concatMap unpack

---------------------------------------------------------------------
-- Creation of yearly journal sets

yearly26Sets :: Int -> T.References -> T.JSets T.Issue
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
-- Working with selection sets for review

selectNone :: T.JSet T.Issue -> T.JSet T.Selection
selectNone (T.JSet setNo xs) = T.JSet setNo . map (flip T.Selection []) $ xs

emptyContent :: T.JSet T.Selection -> T.JSet T.IssueContent
emptyContent jset = T.JSet (T.setNo jset) . map go . T.issues $ jset
    where go = flip T.IssueContent []

restrictContent :: T.IssueContent -> T.IssueContent
restrictContent ic@(T.IssueContent iss cs) = ic { T.citations = cs' }
    where sel = T.selected iss
          cs' = filter ( flip elem sel . T.pmid ) cs

missingPMIDs :: T.IssueContent -> [T.PMID]
-- ^Given an issue content with citations, return the PMIDs of all
-- citations that were selected but are not in the content citations.
missingPMIDs content = sPMIDs \\ cPMIDs
    where cPMIDs = map T.pmid . T.citations $ content
          sPMIDs = T.selected . T.selection $ content

addContent :: [T.Citation] -> [T.Selection] -> ([T.IssueContent], [T.Citation])
-- ^Take a list of selections and a list of citations and add each
-- citation to the appropriate selection in former to generate a list
-- of contents. Return any left-over citations that were not among
-- the selections. Repeated selections will not be populated.
addContent cites = foldr go ([],cites)
    where go x (xs,cs) = bimap (:xs) id $ addCitations x cs

addCitations :: T.Selection -> [T.Citation] -> (T.IssueContent, [T.Citation])
-- ^Add citations to a selection if they have been selected to
-- generate an issue content and return any left over citations.
addCitations sel = bimap (T.IssueContent sel) id . foldr go ([],[])
    where pmids        = T.selected sel
          go c (xs,cs) | elem (T.pmid c) pmids = (c:xs,cs)
                       | otherwise             = (xs,c:cs)

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
