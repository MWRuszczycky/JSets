{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module View.View
    ( runView
      -- Formatting journals and reference issues
    , referenceToTxt
      -- Views of journal sets of Issues
    , viewJSetsIssue
    , jsetsIssueCsv
    , jsetIssueCsv
    , jsetsIssueTxt
    , jsetsIssueMkd
    , jsetIssueMkd
      -- Views of journal sets of Selections
    , viewSelections
    , jsetsSelectionTxt
    , jsetSelectionTxt
      -- Views of journal sets of Contents
    , viewRanks
    , viewToCs
    , jsetContentTxt
    , jsetContentMkd
    , jsetContentHtml
      -- Views of Issues, Selections and Contents
    , showIssue
    , issueTxt
    , issueMkd
    , issuesCsv
    , selectionTxt
    , contentTxt
    , contentMkd
      -- Views of Citations
    , citationTxt
    , citationMkd
      -- Views of match results
    , viewMatches
    ) where

import qualified Data.Text        as Tx
import qualified Data.Map.Strict  as Map
import qualified Model.Core.Core  as C
import qualified Model.Core.Types as T
import qualified Model.Journals   as J
import qualified View.Core        as Vc
import qualified View.Html        as Html
import qualified View.Templates   as Temp
import           Data.Text                ( Text                 )
import           Data.List                ( sortBy               )
import           Control.Monad            ( replicateM_          )
import           Control.Monad.Reader     ( ask, asks, runReader )
import           Control.Monad.Writer     ( execWriterT          )
import           Data.Monoid              ( Endo (..), appEndo   )
import           Data.Ord                 ( comparing            )
import           View.Templates           ( fill                 )

-- =============================================================== --
-- Viewer

runView :: T.ViewMonad a -> T.AppMonad Text
runView view = ask >>= pure . Tx.concat . flip appEndo [] . run
    where run config = flip runReader config . execWriterT $ view

getFormat :: T.ViewMonad T.Format
getFormat = asks ( fmap C.extension . T.cOutputPath )
            >>= \case Just "txt"  -> pure T.TXT
                      Just "csv"  -> pure T.CSV
                      Just "html" -> pure T.HTML
                      Just "mkd"  -> pure T.MKD
                      Just "md"   -> pure T.MKD
                      _           -> pure T.TXT

-- =============================================================== --
-- Formatting journals and reference issues

referenceToTxt :: T.Issue -> Text
referenceToTxt x = Tx.unlines hs
    where j  = T.journal x
          hs = [ T.name j
               , "  abbr:      " <> T.abbr j
               , "  pubmed:    " <> T.pubmed j
               , "  frequency: " <> (freqToTxt . T.freq) j
               , "  resets:    " <> (resetsToTxt . T.resets) j
               , "  reference: " <> showIssue x
               ]

resetsToTxt :: Bool -> Text
resetsToTxt True  = "yes (issue numbers reset to 1 each year)"
resetsToTxt False = "no (issue numbers continuously increase)"

freqToTxt :: T.Frequency -> Text
freqToTxt T.Weekly      = "weekly (52 issues per year)"
freqToTxt T.WeeklyLast  = "weekly-last (drop the last issue of the year)"
freqToTxt T.WeeklyFirst = "weekly-first (drop the first issue of the year)"
freqToTxt T.Monthly     = "monthly (12 issues per year)"

-- =============================================================== --
-- Views of journal sets

viewJSetsIssue :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
viewJSetsIssue jsets = do
    abbrs <- asks T.cReferences >>= pure . map ( T.abbr . T.journal )
    getFormat >>= \case
        T.CSV -> jsetsIssueCsv abbrs jsets
        T.MKD -> jsetsIssueMkd jsets
        _     -> jsetsIssueTxt jsets

---------------------------------------------------------------------
-- As CSV

jsetsIssueCsv :: T.HasIssue a => [Text] -> T.JSets a -> T.ViewMonad ()
-- ^Convert a collection of journal sets to CSV. Every element is
-- enclosed in double quotes. The first line is the list of journals
-- by journal abbreviation as specified by the first argument. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsIssueCsv []    _     = pure ()
jsetsIssueCsv abbrs jsets = do
    Vc.write "No. & Date,"
    Vc.writeLn . Tx.intercalate "," $ abbrs
    mapM_ ( jsetIssueCsv abbrs ) . J.unpack $ jsets

jsetIssueCsv :: T.HasIssue a => [Text] -> T.JSet a -> T.ViewMonad ()
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journal abbreviations in the order they will be
-- tabulated. The second argument is the journal set. The first cell
-- will be the key associated with the journal set. Subsequent cells
-- will list the issues for the corresponding journal separated by
-- newline characters. All elements are enclosed in double quotes.
jsetIssueCsv []    _    = pure ()
jsetIssueCsv abbrs jset = do
    Vc.write "\""
    Vc.write . C.tshow . T.setNo $ jset
    Vc.newLine
    Vc.write . Vc.dateN $ jset
    Vc.write "\","
    Vc.separate (Vc.write ",") . map ( issuesCsv . T.issues $ jset ) $ abbrs
    Vc.newLine

---------------------------------------------------------------------
-- As Text

jsetsIssueTxt :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
jsetsIssueTxt = Vc.separate Vc.newLine . map jsetIssueTxt . J.unpack

jsetIssueTxt :: T.HasIssue a => T.JSet a -> T.ViewMonad ()
jsetIssueTxt jset = do
    let issues = sortBy (comparing $ T.name . T.journal) . T.issues $ jset
    Vc.writeLn $ Vc.jsetHeader jset
    Vc.separate Vc.newLine . map issueTxt $ issues
    Vc.newLine

---------------------------------------------------------------------
-- to Markdown

jsetsIssueMkd :: T.HasIssue a => T.JSets a -> T.ViewMonad ()
jsetsIssueMkd = Vc.separate Vc.newLine . map jsetIssueMkd . J.unpack

jsetIssueMkd :: T.HasIssue a => T.JSet a -> T.ViewMonad ()
jsetIssueMkd jset = do
    Vc.writeLn $ "## " <> Vc.jsetHeader jset
    Vc.prepend (Vc.write "\n* ") . map issueMkd . T.issues $ jset
    Vc.newLine

-- =============================================================== --
-- Formatting selection sets

viewSelections :: T.JSets T.Selection -> T.ViewMonad ()
viewSelections jsets = do
    getFormat >>= \case
        T.TXT -> jsetsSelectionTxt jsets
        _     -> viewJSetsIssue jsets

---------------------------------------------------------------------
-- As text

jsetsSelectionTxt :: T.JSets T.Selection -> T.ViewMonad ()
jsetsSelectionTxt (T.JSets jsets) =
    Vc.separate Vc.newLine . map jsetSelectionTxt $ jsets

jsetSelectionTxt :: T.JSet T.Selection -> T.ViewMonad ()
jsetSelectionTxt jset = do
    Vc.writeLn . Vc.jsetHeader $ jset
    mapM_ selectionTxt . T.issues $ jset

-- =============================================================== --
-- Viewing Issue Contents (tables of contents and ranking lists)

viewRanks :: T.JSet T.Content -> T.ViewMonad ()
viewRanks jset@(T.JSet _ contents) = do
    name  <- maybe "Somebody" id . C.choice <$> mapM asks [T.cNick, T.cUser]
    email <- asks $ maybe "their email address" id . T.cEmail
    getFormat >>= \case
         T.HTML -> Vc.write . Html.rankList name email $ jset
         T.MKD  -> Vc.separate Vc.newLine . map contentMkd $ contents
         _      -> Vc.separate Vc.newLine . map contentTxt $ contents

viewToCs :: T.JSet T.Content -> T.ViewMonad ()
viewToCs jset = do
    getFormat >>= \case
        T.HTML -> jsetContentHtml jset
        T.MKD  -> jsetContentMkd  jset
        _      -> jsetContentTxt  jset

---------------------------------------------------------------------
-- As Text

jsetContentTxt :: T.JSet T.Content -> T.ViewMonad ()
jsetContentTxt (T.JSet _ cs) = Vc.separate Vc.newLine . map contentTxt $ cs

---------------------------------------------------------------------
-- As Markdown

jsetContentMkd :: T.JSet T.Content -> T.ViewMonad ()
jsetContentMkd (T.JSet setNo cs) = do
    Vc.write "# Journal Set "
    Vc.writeLn . C.tshow $ setNo
    Vc.newLine
    Vc.separate Vc.newLine . map contentMkd $ cs

---------------------------------------------------------------------
-- As HTML

jsetContentHtml :: T.JSet T.Content -> T.ViewMonad ()
jsetContentHtml jset = do
    style <- asks T.cToCStyle
    name  <- maybe "Somebody" id . C.choice <$> mapM asks [T.cNick, T.cUser]
    email <- asks $ maybe "their email address" id . T.cEmail
    case style of
         T.Basic   -> Vc.write . Html.tocBasic              $ jset
         T.Select  -> Vc.write . Html.tocSelect  name email $ jset
         T.Propose -> Vc.write . Html.tocPropose name email $ jset

-- =============================================================== --
-- Viewing issues

---------------------------------------------------------------------
-- Raw text

showIssue :: T.HasIssue a => a -> Text
showIssue iss = Tx.unwords . map ($iss) $ parts
    where parts = [ T.abbr . T.journal, Vc.volIss, Vc.dateP ]

---------------------------------------------------------------------
-- Views of Issues

issueTxt :: T.HasIssue a => a -> T.ViewMonad ()
-- ^Format an issue as "abbr volume number (year-month-day)".
issueTxt iss = do
    Vc.write . T.abbr . T.journal $ iss
    Vc.space
    Vc.write . Vc.volIss $ iss
    Vc.space
    Vc.write . Vc.dateP $ iss

issueMkd :: T.HasIssue a => a -> T.ViewMonad ()
issueMkd iss = do
    Vc.write . T.name . T.journal $ iss
    Vc.space
    Vc.write . Vc.volIss $ iss
    Vc.write ","
    Vc.space
    Vc.write . Vc.dateW $ iss

issuesCsv :: T.HasIssue a => [a] -> Text -> T.ViewMonad ()
issuesCsv iss abbr = do
    Vc.write "\""
    Vc.writeLns' . map Vc.volIss . J.issuesByAbbr abbr $ iss
    Vc.write "\""

---------------------------------------------------------------------
-- Views of Selections

selectionTxt :: T.Selection -> T.ViewMonad ()
selectionTxt (T.Selection iss pmids) = do
    let indent = replicateM_ 4 Vc.space
    issueTxt iss
    Vc.newLine
    Vc.prepend indent . map Vc.writeLn $ pmids

---------------------------------------------------------------------
-- Views of Contents

contentTxt :: T.Content -> T.ViewMonad ()
contentTxt (T.Content sel cs) = do
    issueTxt sel
    replicateM_ 2 Vc.newLine
    Vc.separate Vc.newLine . map (citationTxt sel) $ cs

contentMkd :: T.Content -> T.ViewMonad ()
contentMkd (T.Content x cs) = do
    Vc.write "## "
    issueMkd x
    replicateM_ 2 Vc.newLine
    if null cs
       then Vc.writeLn "No citations listed at PubMed."
       else Vc.separate Vc.newLine . map (citationMkd x) $ cs

-- =============================================================== --
-- Viewing citations

citationTxt :: T.HasIssue a => a -> T.Citation -> T.ViewMonad ()
citationTxt iss c = do
    Vc.writeLn . T.title $ c
    Vc.writeLn . Vc.authorLine $c
    Vc.write . T.name . T.journal $ iss
    Vc.space
    Vc.write . Vc.volIss $ iss
    Vc.write ","
    Vc.space
    Vc.write . C.tshow . T.pages $ c
    Vc.newLine

citationMkd :: T.Selection -> T.Citation -> T.ViewMonad ()
citationMkd sel x = Vc.write . fill dict $ Temp.citationMkd
    where dict = Map.fromList [ ( "title",   Vc.mkdBrackets . T.title $ x )
                              , ( "doi",     T.doi x                      )
                              , ( "authors", Vc.authorLine x              )
                              , ( "journal", T.name . T.journal $ sel     )
                              , ( "volIss",  Vc.volIss sel                )
                              , ( "pages",   C.tshow . T.pages $ x        )
                              , ( "pmid",    T.pmid x                     )
                              ]

-- =============================================================== --
-- Viewing match results

viewMatches :: T.MatchResult -> Text
viewMatches result = Tx.unlines $ hdr : matches
    where score     = C.tshow . T.matchScore $ result
          go (n,ms) = "    " <> n <> ": " <> Tx.unwords ms
          hdr       = T.matchTitle result <> ", score: " <> score
          matches   = map go . T.matchings $ result
