{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module View.View
    ( runView
      -- Formatting journals and reference issues
    , viewConfig
    , referenceToTxt
      -- Views of journal sets of Issues
    , viewJSets
    , jsetsCsv
    , jsetCsv
    , jsetsTxt
    , jsetsMkd
    , jsetMkd
      -- Views of journal sets of Contents
    , viewToCs
    , jsetContentTxt
    , jsetContentMkd
    , jsetContentHtml
      -- Views of rankings
    , viewRanks
      -- Views of Issues, Selections and Contents
    , showIssue
    , issueTxt
    , issueMkd
    , issuesCsv
    , contentTxt
    , contentMkd
      -- Views of Citations
    , viewCitations
    , viewCitation
    , citationTxt
    , citationMkd
      -- Views of match results
    , matchTemplate
    , viewMatchResult
    ) where

import qualified Data.Map.Strict  as Map
import qualified Data.Text        as Tx
import qualified Model.Core.Core  as C
import qualified Model.Core.Types as T
import qualified Model.Journals   as J
import qualified View.Core        as Vc
import qualified View.Html        as Html
import qualified View.Templates   as Temp
import           Control.Monad            ( replicateM_, when    )
import           Control.Monad.Reader     ( ask, asks, runReader )
import           Control.Monad.Writer     ( execWriterT          )
import           Data.List                ( sortBy, nub          )
import           Data.Maybe               ( mapMaybe             )
import           Data.Monoid              ( Endo (..), appEndo   )
import           Data.Ord                 ( comparing            )
import           Data.Text                ( Text                 )
import           View.Templates           ( fill                 )

-- =============================================================== --
-- Viewer

runView :: T.ViewMonad a -> T.AppMonad Text
runView view = ask >>= pure . Tx.concat . flip appEndo [] . run
    where run config = flip runReader config . execWriterT $ view

-- =============================================================== --
-- Viewing configuration files and configured references

viewConfig :: T.ViewMonad ()
viewConfig = do
    asks T.cConfigPath >>= Vc.writeLn . configPathTxt
    Vc.newLine
    (,) <$> asks T.cTerse <*> asks T.cReferences >>= \case
         ( _,     [] ) -> Vc.writeLn "There are no references configured."
         ( False, rs ) -> Vc.writeLns' . map referenceToTxt $ rs
         ( True,  rs ) -> terseReferencesAsTxt rs

configPathTxt :: Maybe FilePath -> Text
configPathTxt (Just path) = "Configuration file: " <> Tx.pack path
configPathTxt Nothing     = Tx.intercalate "\n" msg
    where msg = [ "No configuration file provided."
                , "Normally this file should be ~/.config/jsets/config"
                ]

referenceToTxt :: T.Issue -> Text
referenceToTxt x = Tx.unlines hs
    where j  = T.journal x
          hs = [ T.name j
               , "  abbr:      " <> T.abbr j
               , "  pubmed:    " <> T.pubmed j
               , "  followed:  " <> (flagToTxt . T.followed) j
               , "  frequency: " <> (freqToTxt . T.freq) j
               , "  resets:    " <> (resetsToTxt . T.resets) j
               , "  reference: " <> showIssue x
               , "  mincount:  " <> (C.tshow . T.mincount) j
               ]

terseReferencesAsTxt :: T.References -> T.ViewMonad ()
terseReferencesAsTxt rs = hdr *> mapM_ go rs
    where hdr   = Vc.writeLn "Journal Name/Abbreviation/PubMed Name\n"
          go x  = Vc.writeLn . Tx.intercalate "/"
                             . map ($ T.journal x) $ [ T.name
                                                     , T.abbr
                                                     , T.pubmed
                                                     ]

resetsToTxt :: Bool -> Text
resetsToTxt True  = "yes (issue numbers reset to 1 each year)"
resetsToTxt False = "no (issue numbers continuously increase)"

flagToTxt :: Bool -> Text
flagToTxt True  = "yes"
flagToTxt False = "no"

freqToTxt :: T.Frequency -> Text
freqToTxt T.WeeklyLast  = "weekly-last (drop the last issue of the year)"
freqToTxt T.WeeklyFirst = "weekly-first (drop the first issue of the year)"
freqToTxt T.Monthly     = "monthly near beginning of the month (12 issues per year)"
freqToTxt T.MidMonthly  = "monthly around mid-month (12 issues per year)"
freqToTxt T.EndMonthly  = "monthly by the end of every month (12 issues per year)"
freqToTxt T.SemiMonthly = "semimonthly at regular intervals (24 issues per year)"
freqToTxt T.UnknownFreq = "unknown (assumed every week)"
freqToTxt (T.EveryNWeeks n)
    | n < 1     = "unknown (assumed every week)"
    | n == 1    = "weekly (52 issues per year)"
    | otherwise = "every " <> C.tshow n <> " weeks"

-- =============================================================== --
-- Views of journal sets

viewJSets :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
viewJSets jsets = do
    abbrs <- asks T.cReferences >>= pure . map ( T.abbr . T.journal )
    asks T.cFormat >>= \case
        T.CSV -> jsetsCsv abbrs jsets
        T.MKD -> jsetsMkd jsets
        _     -> jsetsTxt jsets

---------------------------------------------------------------------
-- As CSV

jsetsCsv :: T.HasIssue a => [Text] -> T.JSets a -> T.ViewMonad ()
-- ^Convert a collection of journal sets to CSV. Every element is
-- enclosed in double quotes. The first line is the list of journals
-- by journal abbreviation as specified by the first argument. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsCsv []    _     = pure ()
jsetsCsv abbrs jsets = do
    Vc.write "No. & Date,"
    Vc.writeLn . Tx.intercalate "," $ abbrs
    mapM_ ( jsetCsv abbrs ) . J.unpack $ jsets

jsetCsv :: T.HasIssue a => [Text] -> T.JSet a -> T.ViewMonad ()
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journal abbreviations in the order they will be
-- tabulated. The second argument is the journal set. The first cell
-- will be the key associated with the journal set. Subsequent cells
-- will list the issues for the corresponding journal separated by
-- newline characters. All elements are enclosed in double quotes.
jsetCsv []    _    = pure ()
jsetCsv abbrs jset = do
    Vc.write "\""
    Vc.write . C.tshow . T.setNo $ jset
    Vc.newLine
    Vc.write . Vc.dateN $ jset
    Vc.write "\","
    Vc.separate (Vc.write ",") . map ( issuesCsv . T.issues $ jset ) $ abbrs
    Vc.newLine

---------------------------------------------------------------------
-- As Text

jsetsTxt :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
jsetsTxt = Vc.separate Vc.newLine . map jsetTxt . J.unpack

jsetTxt :: T.HasIssue a => T.JSet a -> T.ViewMonad ()
jsetTxt jset = do
    shouldSort <- asks T.cSortJSets
    let xs = if shouldSort
                then sortBy (comparing $ T.name . T.journal) . T.issues $ jset
                else T.issues jset
    Vc.writeLn $ Vc.jsetHeader jset
    Vc.separate Vc.newLine . map issueTxt $ xs
    Vc.newLine

---------------------------------------------------------------------
-- to Markdown

jsetsMkd :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
jsetsMkd = Vc.separate Vc.newLine . map jsetMkd . J.unpack

jsetMkd :: T.HasIssue a => T.JSet a -> T.ViewMonad ()
jsetMkd jset = do
    Vc.writeLn $ "## " <> Vc.jsetHeader jset
    Vc.prepend (Vc.write "\n* ") . map issueMkd . T.issues $ jset
    Vc.newLine

-- =============================================================== --
-- Viewing tables of contents

viewToCs :: T.Citations -> T.JSet T.ToC -> T.ViewMonad ()
viewToCs cs jset = asks T.cFormat >>= \case
                       T.HTML -> jsetContentHtml cs jset
                       T.MKD  -> jsetContentMkd  cs jset
                       _      -> jsetContentTxt  cs jset

---------------------------------------------------------------------
-- As Text

jsetContentTxt :: T.Citations -> T.JSet T.ToC -> T.ViewMonad ()
jsetContentTxt cs (T.JSet _ xs _) =
    Vc.separate Vc.newLine . map (contentTxt cs) $ xs

---------------------------------------------------------------------
-- As Markdown

jsetContentMkd :: T.Citations -> T.JSet T.ToC -> T.ViewMonad ()
jsetContentMkd cs (T.JSet setNo xs _) = do
    Vc.write "# Journal Set "
    Vc.writeLn . C.tshow $ setNo
    Vc.newLine
    Vc.separate Vc.newLine . map (contentMkd cs) $ xs

---------------------------------------------------------------------
-- As HTML

jsetContentHtml :: T.Citations -> T.JSet T.ToC -> T.ViewMonad ()
jsetContentHtml cs jset = do
    name  <- asks T.cUser
    email <- asks T.cEmail
    date  <- asks T.cDate
    Vc.write . Html.tocsHtml name email date jset $ cs

-- ==================================================================
-- Viewing rankings

viewRanks :: T.Citations -> T.JSet a -> T.ViewMonad ()
viewRanks cs jset@(T.JSet _ _ sel) = do
    name  <- asks T.cUser
    email <- asks T.cEmail
    date  <- asks T.cDate
    let pmids = J.pmidsInSelection sel
    let go = flip Map.lookup cs
    asks T.cFormat >>= \case
         T.HTML -> Vc.write . Html.ranksHtml name email date cs $ jset
         T.MKD  -> Vc.separate Vc.newLine . map citationMkd . mapMaybe go $ pmids
         _      -> Vc.separate Vc.newLine . map citationTxt . mapMaybe go $ pmids

-- =============================================================== --
-- Viewing issues

---------------------------------------------------------------------
-- Raw text

showIssue :: T.HasIssue a => a -> Text
showIssue x = Tx.unwords . map ($ x) $ parts
    where parts = [ T.abbr . T.journal, Vc.volIss, Vc.dateP ]

---------------------------------------------------------------------
-- Views of Issues

issueTxt :: T.HasIssue a => a -> T.ViewMonad ()
-- ^Format an issue as "abbr volume number (year-month-day)".
issueTxt x = do
    Vc.write . T.abbr . T.journal $ x
    Vc.space
    Vc.write . Vc.volIss $ x
    Vc.space
    Vc.write . Vc.dateP $ x

issueMkd :: T.HasIssue a => a -> T.ViewMonad ()
issueMkd x = do
    Vc.write . T.name . T.journal $ x
    Vc.space
    Vc.write . Vc.volIss $ x
    Vc.write ","
    Vc.space
    Vc.write . Vc.dateW $ x

issuesCsv :: T.HasIssue a => [a] -> Text -> T.ViewMonad ()
issuesCsv x abbr = do
    Vc.write "\""
    Vc.writeLns' . map Vc.volIss . J.issuesByAbbr abbr $ x
    Vc.write "\""

---------------------------------------------------------------------
-- Views of Contents

contentTxt :: T.Citations -> T.ToC -> T.ViewMonad ()
contentTxt cs (T.ToC x _ pmids) = do
    issueTxt x
    replicateM_ 2 Vc.newLine
    Vc.separate Vc.newLine . map citationTxt
                           . mapMaybe (flip Map.lookup cs)
                           $ pmids

contentMkd :: T.Citations -> T.ToC -> T.ViewMonad ()
contentMkd cs (T.ToC x _ pmids) = do
    Vc.write "## " *> issueMkd x
    replicateM_ 2 Vc.newLine
    if null cs
       then Vc.writeLn "No citations listed at PubMed."
       else Vc.separate Vc.newLine . map citationMkd
                                   . mapMaybe (flip Map.lookup cs)
                                   $ pmids

-- =============================================================== --
-- Viewing citations

viewCitations :: [T.Citation] -> T.ViewMonad ()
viewCitations = Vc.separate Vc.newLine . map viewCitation

viewCitation :: T.Citation -> T.ViewMonad ()
viewCitation x = asks T.cFormat >>= \case
                     T.MKD -> citationMkd x
                     _     -> citationTxt x

citationTxt :: T.Citation -> T.ViewMonad ()
citationTxt x = do
    Vc.writeLn . T.title $ x
    Vc.writeLn . Vc.authorLine $ x
    Vc.write . T.name . T.journal . T.issue $ x
    Vc.space
    Vc.write . Vc.volIss . T.issue $ x
    Vc.write ","
    Vc.space
    Vc.write . C.tshow . T.pages $ x
    Vc.space
    Vc.write . Vc.bracket '(' ')' . T.pmid $ x
    Vc.newLine

citationMkd :: T.Citation -> T.ViewMonad ()
citationMkd x = Vc.write . fill dict $ Temp.citationMkd
    where dict = Map.fromList [ ( "title",   Vc.mkdBrackets . T.title $ x     )
                              , ( "doi",     T.doi x                          )
                              , ( "authors", Vc.authorLine x                  )
                              , ( "journal", T.name . T.journal . T.issue $ x )
                              , ( "volIss",  Vc.volIss . T.issue $ x          )
                              , ( "pages",   C.tshow . T.pages $ x            )
                              , ( "pmid",    T.pmid x                         )
                              ]

-- =============================================================== --
-- Viewing match results

matchTemplate :: T.JSet a -> T.ViewMonad ()
-- ^Gnerate blank rank-lists file for use with the match command.
matchTemplate jset = do
    let count = length . T.selection $ jset
        setno = C.tshow . T.setNo    $ jset
        names = ["red:", "green:", "blue:"]
    dateStr <- asks $ Vc.dateW . T.cDate
    Vc.writeLn $ "# Rank-lists for Journal Set " <> setno <> ", " <> dateStr
    Vc.newLine
    Vc.writeLn "# The papers to be matched"
    Vc.writeLn "title: papers"
    Vc.writeLn . ("    " <>) . Tx.unwords . map C.tshow $ [1 .. count]
    Vc.newLine
    Vc.writeLn "# The rank-listed preferences for each person"
    Vc.writeLns names

viewMatchResult :: T.MatchResult -> T.ViewMonad ()
viewMatchResult (T.MatchResult t _  _   _ (Left err)) = do
    Vc.write t
    Vc.write ", match failed: "
    Vc.write . Tx.pack $ err
    Vc.newLine
viewMatchResult (T.MatchResult t ps ids ss (Right (s,xs))) = do
    let indent = replicateM_ 2 Vc.space
    Vc.write t
    Vc.writeLn $ ", score: " <> C.tshow s
    showDetails <- asks T.cMatchDetails
    when showDetails $ do
        indent *> Vc.write "papers: "
        Vc.separate Vc.space . map ( Vc.write . C.tshow ) . filter (>0) $ ps
        Vc.newLine
        indent *> Vc.writeLn "scores:"
        Vc.separate Vc.newLine . map (viewScores ss) $ ids
        Vc.newLine
    indent *> Vc.writeLn "matches:"
    mapM_ (viewMatches xs) ids

viewScores :: [((Int,Int),Int)] -> (Text, [Int]) -> T.ViewMonad ()
viewScores _ (name, []) = do
    replicateM_ 4 Vc.space
    Vc.write name *> Vc.write ": " *> Vc.write"unmatched"
viewScores ss (name, x:_) = do
    let f p | p < 1 = "none" | otherwise = C.tshow p
        g (p,s) = p <> ":" <> C.tshow s
    replicateM_ 4 Vc.space *> Vc.write name *> Vc.write ": "
    Vc.separate Vc.space . map ( Vc.write . g )
                         . reverse
                         . sortBy (comparing snd)
                         $ nub [ ( f p, s ) | ( (p,i), s ) <- ss, i == x ]

viewMatches :: [(Int,Int)] -> (Text,[Int]) -> T.ViewMonad ()
viewMatches ms (name, ids) = do
    let matches = [ p | (p,i) <- ms, elem i ids, p > 0 ]
    replicateM_ 4 Vc.space
    Vc.write name *> Vc.write ": "
    if null matches
       then Vc.write "none"
       else Vc.write . Tx.unwords . map C.tshow $ matches
    Vc.newLine
