{-# LANGUAGE OverloadedStrings #-}

module Model.Journals
    ( -- Working with citations and selections
      isPMID
    , pmidsInSelection
    , pmidsInSelectionFree
    , updateToC
    , correctCitation
    , resolveCitationIssue
      -- Working with journal sets
    , pack
    , unpack
    , emptyJSets
    , lookupJSet
    , combineJSets
    , yearlySetsByDate
    , yearlySets
    , issuesByAbbr
    , isFollowedWeekly
    , isFollowedMonthly
    , isFollowedOther
      -- Working with journal issues
    , issueAtDate
    , issuesFromDate
    , issuesByDates
    , issuesInYear
    , issuesInVolume
    , lookupIssue
    , lookupIssueInYear
    , nextIssue
    ) where

import qualified Model.Core.Core      as C
import qualified Model.Core.Dates     as D
import qualified Model.Core.Types     as T
import qualified Data.Time            as Tm
import           Data.Maybe                  ( mapMaybe     )
import           Data.List                   ( find, foldl' )
import           Data.Time                   ( Day          )
import           Data.Text                   ( Text         )
import           Data.List                   ( sortOn       )

-- =============================================================== --
-- Working with citations and selections

isPMID :: T.Selection -> Bool
isPMID (T.ByBndPMID _ _) = True
isPMID (T.ByPMID      _) = True
isPMID _                 = False

pmidsInSelection :: [T.Selection] -> [T.PMID]
pmidsInSelection = mapMaybe go
    where go (T.ByBndPMID _ x) = Just x
          go (T.ByPMID      x) = Just x
          go _                 = Nothing

pmidsInSelectionFree :: [T.Selection] -> [T.PMID]
pmidsInSelectionFree = mapMaybe go
    where go (T.ByPMID x) = Just x
          go _            = Nothing

updateToC :: [T.Selection] -> T.ToC -> T.ToC
-- ^Add any selected PMID that is bound to a specific Issue to that
-- issue's table of contents if not already present. This function
-- is useful when an issue is published but PubMed still thinks that
-- all of its citations are ahead of print. So, the citation and
-- table of contents have to be obtained separately and recombined.
updateToC sel x = x { T.contents = foldl' go (T.contents x) sel }
    where go ps (T.ByBndPMID i p) | i == T.issue x && (not . elem p ) ps = p:ps
                                  | otherwise                            = ps
          go ps _                 = ps

correctCitation :: T.References -> [T.ToC] -> T.Citation -> T.Citation
-- ^Correct the issue of a citation if the journal is configured and
-- determine whether the citation is an extra-citation.
correctCitation rs tocs c = maybe resolved id . C.choice . map go $ tocs
    where resolved = resolveCitationIssue rs $ c { T.isExtra = True }
          go toc   = if elem (T.pmid c) . T.contents $ toc
                        then Just $ c { T.pubIssue = T.issue toc
                                      , T.isExtra  = False }
                        else Nothing

resolveCitationIssue :: T.References -> T.Citation -> T.Citation
-- ^Attempt to resolve the issue of a citation to a configured issue.
resolveCitationIssue refs x = maybe x id rx
    where name = T.pubmed . T.journal $ x
          rx   = do r   <- find ( (== name) . T.pubmed . T.journal ) refs
                    let key = T.abbr . T.journal $ r
                    iss <- lookupIssue refs key ( T.volNo x, T.issNo x )
                    pure $ x { T.pubIssue = iss }

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

yearlySetsByDate :: Int -> Int -> T.References -> T.JSets T.Issue
-- Compute the issues in each journal set for a specified year given
-- some frequency k (in weeks) of the journal sets. Distribute the
-- sets reasonably evenly by publication date.
yearlySetsByDate y k refs
    | y < 2000  = pack []
    | k < 1     = pack []
    | otherwise = let jsets = filter ( not . null ) . groupInYear y k $ refs
                  in  pack [ T.JSet n i [] | (n, i) <- zip [1..] jsets ]

yearlySets :: Int -> Int -> T.References -> T.JSets T.Issue
-- Compute the issues in each journal set for a specified year given
-- some frequency k (in weeks) of the journal sets. This function
-- tries to form the journal sets such that an equal number of each
-- weekly journal shows up in every journal set, with extras pushed
-- towards the end. This provides a more even distribution of the
-- issues; however, it can lead to journal set dates being later
-- than they would otherwise need to be.
yearlySets y k refs
    | y < 2000  = pack []
    | k < 1     = pack []
    | otherwise =
        let notWeekly x = isFollowedMonthly x || isFollowedOther x
            wsets       = weeklyInYear y k . filter isFollowedWeekly $ refs
            xsets       = groupInYear  y k . filter notWeekly        $ refs
            sets        = filter ( not . null ) $ C.zipLists wsets xsets
        in  pack [ T.JSet n i [] | (n, i) <- zip [1..] sets ]

setsInYear :: Int -> Int
-- ^If journal sets are reviewed every k weeks, compute how many
-- journal sets will be required in a given year.
setsInYear k
    | k < 1     = 0
    | r == 0    = q
    | otherwise = q + 1
    where (q,r) = quotRem 52 k

weeklyInYear :: Int -> Int -> T.References -> [[T.Issue]]
-- ^Group all weekly issues published in a given year by trying to
-- keep an equal number of issues from each weekly journal in every
-- subset. This usually works, because the journal set frequency
-- cannot be more frequent than once per week. Only references with
-- weekly publication frequencies should be provided as input.
weeklyInYear y k = foldr C.zipLists start . map (groupInChunks y k)
    where start = replicate (setsInYear k) []

groupInYear :: Int -> Int -> T.References -> [[T.Issue]]
-- ^Group all issues published in a given year reasonably evenly into
-- sets at a frequency of k weeks after sorting by date.
groupInYear y k = groupIssues n . sortOn T.date . concatMap (issuesInYear y)
    where n = setsInYear k

-- ------------------------------------------------------------------ 

groupInChunks :: Int -> Int -> T.Issue -> [[T.Issue]]
-- ^Compute all the issues for a journal in the year y and then chunk
-- it into groups of at least k issues. This is useful for getting
-- the issues of each weekly issue that will in a given journal set.
groupInChunks y k x = xs <> replicate ( setsInYear k - length xs ) []
    where xs = C.chunksOf k . issuesInYear y $ x

groupIssues :: Int -> [a] -> [[a]]
-- Group the elements of a list of into n sublists so that they are
-- more-or-less evenly distributed. The elements are distributed to
-- push larger sublists towards the end of the list. This helps to
-- ensure journal sets become available as soon as possible. The
-- length of the returned list is n or 0 if n < 1.
-- n   : site of the list to be created (i.e., the number of bins)
-- q   : min number monthly issues per set
-- q+1 : max number monthly issues per set, there will be r of these
-- v   : initial number of sets with only q issues each
-- u   : after the first v sets, we have u - 1 sets with only q
--       monthly issues than one set with q + 1 sets & this repeats.
groupIssues n xs
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

---------------------------------------------------------------------
-- Helper functions

issuesByAbbr :: T.HasIssue a => Text -> [a] -> [a]
-- ^Pull all issues in a list for a given journal abbr.
issuesByAbbr abbr = filter ( (== abbr) . T.abbr . T.journal )

isFollowedWeekly :: T.Issue -> Bool
-- ^The issue has a weekly publication frequency and is followed.
isFollowedWeekly x = let followed = T.followed . T.journal $ x
                     in  case T.freq . T.journal $ x of
                              T.EveryNWeeks 1 -> followed
                              T.WeeklyLast    -> followed
                              T.WeeklyFirst   -> followed
                              _               -> False

isFollowedMonthly :: T.Issue -> Bool
-- ^The issue is a monthly publication and is followed.
isFollowedMonthly x = case T.freq . T.journal $ x of
                           T.Monthly -> T.followed . T.journal $ x
                           _         -> False

isFollowedOther :: T.Issue -> Bool
-- ^The is not published weekly or monthly and is followed.
isFollowedOther x = let followed = T.followed . T.journal $ x
                    in  case T.freq . T.journal $ x of
                             T.EveryNWeeks n -> followed && n > 1
                             _               -> False

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
issueAtDate d = go . iterate nextIssue
    where go ~(x0:x1:xs) | T.theDate x1 >= d = x0
                         | otherwise         = go (x1:xs)

issuesByDates :: Day -> Day -> T.Issue -> [T.Issue]
-- ^List of all issues available at the dates provided and in between.
-- The reference issue must be available at the first date.
issuesByDates d0 d1 ref
    | d1 > d1   = []
    | otherwise = takeWhile ( (<= d1) . T.theDate ) . issuesFromDate d0 $ ref

issuesFromDate :: Day -> T.Issue -> [T.Issue]
-- ^List of all issues from a given date on. The reference issue must
-- be available by the start date.
issuesFromDate d0 ref = iterate nextIssue . issueAtDate d0 $ ref

issuesInYear :: Int -> T.Issue -> [T.Issue]
-- ^All issues with publication dates in the specified year.
issuesInYear y x = take 52 . filter ((== y) . T.year) $ xs
    where d0 = Tm.fromGregorian (fromIntegral   y    ) 1  2
          d1 = Tm.fromGregorian (fromIntegral $ y + 1) 1  1
          xs = issuesByDates d0 d1 x

issuesInVolume :: Int -> T.Issue -> [T.Issue]
issuesInVolume v = takeWhile ((==v) . T.theVolNo)
                   . dropWhile ((<v) . T.theVolNo)
                   . iterate nextIssue

lookupIssue :: T.References -> Text -> (Int, Int) -> Maybe T.Issue
lookupIssue refs abbr (v,n) =
    find ( (== abbr) . T.abbr . T.theJournal ) refs
    >>= find ( (== n) . T.issNo ) . issuesInVolume v

lookupIssueInYear :: T.References -> Text -> (Int, Int) -> Maybe T.Issue
lookupIssueInYear refs abbr (y,n) =
    find ( (== abbr) . T.abbr . T.journal ) refs
    >>= find ( (== n) . T.issNo ) . issuesInYear y

nextIssue :: T.Issue -> T.Issue
-- ^Compute the next issue based on the the publication frequency.
-- If the frequency is unknown, then assume it is every week.
nextIssue x = case T.freq . T.theJournal $ x of
                   T.Monthly       -> nextMonthly       x
                   T.WeeklyFirst   -> nextWeekly        x
                   T.WeeklyLast    -> nextWeekly        x
                   T.EveryNWeeks n -> nextEveryNWeeks n x
                   T.UnknownFreq   -> nextEveryNWeeks 1 x

-- ------------------------------------------------------------------ 
-- Computing next issues

nextMonthly :: T.Issue -> T.Issue
-- ^Compute the next monthly issue. The journal must have a Monthly
-- publication frequency.
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
-- ^Compute the next issue for journals having special weekly
-- publication frequencies (i.e., either the first or last issue of
-- every years is dropped). Journal must have one of these two
-- publication frequencies.
nextWeekly x1
    | D.sameYear d1 d3 = x2 { T.theDate = d2 }
    | dropLast         = x2 { T.theDate = d3, T.theVolNo = v2, T.theIssNo = n2y }
    | D.sameYear d1 d2 = x2 { T.theDate = d2 }
    | otherwise        = x2 { T.theDate = d3, T.theVolNo = v2, T.theIssNo = n2y }
    where dropLast = (T.freq . T.theJournal) x1 == T.WeeklyLast
          d1       = T.theDate x1
          d2       = Tm.addDays 7 d1
          d3       = Tm.addDays 14 d1
          v2       = succ . T.theVolNo $ x1
          n2       = succ . T.theIssNo $ x1
          x2       = x1 { T.theIssNo = n2 }
          n2y      = if T.resets . T.theJournal $ x1 then 1 else n2

nextEveryNWeeks :: Int -> T.Issue -> T.Issue
-- ^Compute the next issue for journals publishing exactly every n
-- weeks. If n < 1, then it is assumed to be weekly. This ensures
-- that interation terminates.
nextEveryNWeeks n x1
    | n < 1            = nextEveryNWeeks 1 x1
    | D.sameYear d1 d2 = x2 { T.theVolNo = v1     , T.theIssNo = succ i1 }
    | otherwise        = x2 { T.theVolNo = succ v1, T.theIssNo = i2      }
    where d1 = T.theDate x1
          d2 = Tm.addDays (fromIntegral $ 7 * n) d1
          x2 = x1 { T.theDate = d2 }
          v1 = T.theVolNo $ x1
          i1 = T.theIssNo $ x1
          i2 = if T.resets . T.theJournal $ x1 then 1 else succ i1
