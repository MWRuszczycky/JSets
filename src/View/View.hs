{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module View.View
    ( runView
      -- Viewing journal sets
    , jsetsToCsv
    , jsetToCsv
    , jsetsToTxt
    , jsetToTxt
    , jsetsToMkd
    , jsetToMkd
      -- Viewing Issues
    , issueToTxt
    , issueToMkd
      -- Viewing citations
    , citationToTxt
    , citationToMkd
      -- Viewing issue contents (tables of contents)
    , viewRanks
    , tocsToTxt
    , tocToTxt
    , tocsToMkd
    , tocToMkd
    , tocsToHtml
      -- Viewing selections
    , selectionsToTxt
    , selectionToTxt
      -- Viewing references
    , referenceToTxt
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
import           Control.Monad.Reader     ( ask, asks, runReader )
import           Data.Ord                 ( comparing            )
import           View.Templates           ( fill                 )

-- =============================================================== --
-- Viewer

runView :: T.ViewMonad a -> T.AppMonad a
runView go = ask >>= pure . runReader go

format :: T.ViewMonad T.Format
format = asks ( fmap C.extension . T.cOutputPath )
         >>= \case Just "txt"  -> pure T.TXT
                   Just "csv"  -> pure T.CSV
                   Just "html" -> pure T.HTML
                   Just "mkd"  -> pure T.MKD
                   Just "md"   -> pure T.MKD
                   _           -> pure T.TXT

-- =============================================================== --
-- Viewing journal sets

---------------------------------------------------------------------
-- As CSV

jsetsToCsv :: T.HasIssue a => [Text] -> T.JSets a -> Text
-- ^Convert a collection of journal sets to CSV. Every element is
-- enclosed in double quotes. The first line is the list of journals
-- by journal abbreviation as specified by the first argument. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsToCsv abbrs col = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (jsetToCsv abbrs) . J.unpack $ col
              hdr  = Tx.intercalate "," $ "No. & Date" : abbrs

jsetToCsv :: T.HasIssue a => [Text] -> T.JSet a -> Text
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journal abbreviations in the order they will be
-- tabulated. The second argument is the journal set. The first cell
-- will be the key associated with the journal set. Subsequent cells
-- will list the issues for the corresponding journal separated by
-- newline characters. All elements are enclosed in double quotes.
jsetToCsv abbrs jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ abbrs
    where go k xs = (volIss . J.issuesByAbbr k . T.issues) jset : xs
          volIss  = Vc.bracket '\"' '\"' . Tx.intercalate "\n" . map Vc.volIss
          hdr     = Vc.bracket '\"' '\"' ( setNo <> "\n" <> date ) <> ","
          date    = Vc.dateN          $ jset
          setNo   = C.tshow . T.setNo $ jset

---------------------------------------------------------------------
-- As Text

jsetsToTxt :: T.HasIssue a => T.JSets a -> Text
jsetsToTxt = Tx.intercalate "\n" . map jsetToTxt . J.unpack

jsetToTxt :: T.HasIssue a => T.JSet a -> Text
-- ^Convert a journal set to easily readable, formatted text.
jsetToTxt jset = Vc.jsetHeader jset <> "\n" <> Tx.unlines xs
    where xs    = map issueToTxt . sortBy (comparing jName) . T.issues $ jset
          jName = T.name . T.journal

---------------------------------------------------------------------
-- to Markdown

jsetsToMkd :: T.HasIssue a => T.JSets a -> Text
jsetsToMkd = Tx.unlines . map jsetToMkd . J.unpack

jsetToMkd :: T.HasIssue a => T.JSet a -> Text
jsetToMkd jset = Tx.concat $ hdr : iss
    where hdr = "## " <> Vc.jsetHeader jset <> "\n\n"
          iss = map ( Tx.append "* " . issueToMkd ) . T.issues $ jset

-- =============================================================== --
-- Viewing issues

---------------------------------------------------------------------
-- As Text

issueToTxt :: T.HasIssue a => a -> Text
-- ^Format an issue as "abbr volume number (year-month-day)".
issueToTxt iss = Tx.unwords . map ($iss) $ [ T.abbr  . T.journal
                                           , Vc.volIss
                                           , Vc.dateP
                                           ]

issueToMkd :: T.HasIssue a => a -> Text
issueToMkd iss = Tx.unwords [ T.name . T.journal $ iss
                            , Vc.volIss iss <> ","
                            , Vc.dateW  iss <> "\n"
                            ]

-- =============================================================== --
-- Viewing citations

citationToTxt :: T.HasIssue a => a -> T.Citation -> Text
citationToTxt iss c = Tx.unlines parts
    where jrnl  = T.journal iss
          parts = [ T.title c
                  , Vc.authorLine c
                  , Tx.unwords [ T.name jrnl
                               , Vc.volIss iss <> ","
                               , Vc.pageRange c
                               ]
                  ]

citationToMkd :: T.Selection -> T.Citation -> Text
citationToMkd sel x = fill dict Temp.citationMkd
    where dict = Map.fromList [ ( "title",   Vc.mkdBrackets . T.title $ x )
                              , ( "doi",     T.doi x                      )
                              , ( "authors", Vc.authorLine x              )
                              , ( "journal", T.name . T.journal $ sel     )
                              , ( "volIss",  Vc.volIss sel                )
                              , ( "pages",   Vc.pageRange x               )
                              , ( "pmid",    T.pmid x                     )
                              ]

-- =============================================================== --
-- Viewing Issue Contents (tables of contents and ranking lists)

viewRanks :: T.JSet T.IssueContent -> T.ViewMonad Text
viewRanks jset@(T.JSet _ ics) = do
    name  <- maybe "Somebody" id . C.choice <$> mapM asks [T.cNick, T.cUser]
    email <- asks $ maybe "their email address" id . T.cEmail
    format >>= \case
        T.HTML -> pure . Html.htmlToCRank name email $ jset
        T.MKD  -> Tx.intercalate "\n" <$> mapM tocToMkd ics
        _      -> Tx.intercalate "\n" <$> mapM tocToTxt ics

---------------------------------------------------------------------
-- As Text

tocsToTxt :: T.JSet T.IssueContent -> T.ViewMonad Text
tocsToTxt (T.JSet _ cs) = Tx.intercalate "\n" <$> mapM tocToTxt cs

tocToTxt :: T.IssueContent -> T.ViewMonad Text
tocToTxt (T.IssueContent sel cs) = pure . Tx.intercalate "\n"
                                        . (issueToTxt sel <> "\n" :)
                                        . map ( citationToTxt sel ) $ cs

---------------------------------------------------------------------
-- As Markdown

tocsToMkd :: T.JSet T.IssueContent -> T.ViewMonad Text
tocsToMkd (T.JSet setNo cs) = Tx.unlines . (:) hdr <$> mapM tocToMkd cs
    where hdr = "# Journal Set " <> C.tshow setNo

tocToMkd :: T.IssueContent -> T.ViewMonad Text
tocToMkd (T.IssueContent x cs)
    | null cs   = pure $ "## " <> issueToMkd x <> msg
    | otherwise = pure $ "## " <> xs
    where xs  = Tx.unlines . (issueToMkd x :) . map (citationToMkd x) $ cs
          msg = "\nNo citations listed at PubMed.\n"

---------------------------------------------------------------------
-- As HTML

tocsToHtml :: T.JSet T.IssueContent -> T.ViewMonad Text
tocsToHtml jset = do
    style <- asks T.cToCStyle
    name  <- maybe "Somebody" id . C.choice <$> mapM asks [T.cNick, T.cUser]
    email <- asks $ maybe "their email address" id . T.cEmail
    case style of
         T.Select  -> pure . Html.htmlToCSelect  name email $ jset
         T.Rank    -> pure . Html.htmlToCRank    name email $ jset
         T.Propose -> pure . Html.htmlToCPropose name email $ jset

-- =============================================================== --
-- Formatting selection sets

---------------------------------------------------------------------
-- As text

selectionsToTxt :: T.JSets T.Selection -> Text
selectionsToTxt (T.JSets jsets) = Tx.intercalate "\n"
                                  . map selectionToTxt
                                  $ jsets

selectionToTxt :: T.JSet T.Selection -> Text
selectionToTxt jset@(T.JSet _ xs) =
    let go x = issueToTxt x : map ( Tx.append "    " ) (T.selected x)
    in  Tx.unlines $ Vc.jsetHeader jset : concatMap go xs

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
               , "  reference: " <> issueToTxt x
               ]

resetsToTxt :: Bool -> Text
resetsToTxt True  = "yes (issue numbers reset to 1 each year)"
resetsToTxt False = "no (issue numbers continuously increase)"

freqToTxt :: T.Frequency -> Text
freqToTxt T.Weekly      = "weekly (52 issues per year)"
freqToTxt T.WeeklyLast  = "weekly-last (drop the last issue of the year)"
freqToTxt T.WeeklyFirst = "weekly-first (drop the first issue of the year)"
freqToTxt T.Monthly     = "monthly (12 issues per year)"
