{-# LANGUAGE OverloadedStrings #-}

module View.View
    ( -- Viewing journal sets
      jsetsToCsv
    , jsetToCsv
    , jsetsToTxt
    , jsetToTxt
    , jsetsToMkd
    , jsetToMkd
      -- Viewing Issues
    , issueToTxt
    , issueToMkd
      -- Viewing cations
    , citationToTxt
    , citationToMkd
      -- Viewing tables of contents
    , tocsToTxt
    , tocToTxt
    , tocsToHtml
    , tocsToMkd
    , tocToMkd
      -- Viewing selections
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
import           Data.Text                ( Text      )
import           Data.List                ( sortBy    )
import           Data.Ord                 ( comparing )
import           View.Templates           ( fill      )

-- =============================================================== --
-- Viewing journal sets

---------------------------------------------------------------------
-- As CSV

jsetsToCsv :: T.HasIssue a => [Text] -> T.Collection a -> Text
-- ^Convert a collection of journal sets to CSV. Every element is
-- enclosed in double quotes. The first line is the list of journals
-- by journal abbreviation as specified by the first argument. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsToCsv abbrs col = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (jsetToCsv abbrs) . J.unpack $ col
              hdr  = Tx.intercalate "," $ "No. & Date" : abbrs

jsetToCsv :: T.HasIssue a => [Text] -> T.JournalSet a -> Text
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

jsetsToTxt :: T.HasIssue a => T.Collection a -> Text
jsetsToTxt = Tx.intercalate "\n" . map jsetToTxt . J.unpack

jsetToTxt :: T.HasIssue a => T.JournalSet a -> Text
-- ^Convert a journal set to easily readable, formatted text.
jsetToTxt jset = Vc.jsetHeader jset <> "\n" <> Tx.unlines xs
    where xs    = map issueToTxt . sortBy (comparing jName) . T.issues $ jset
          jName = T.name . T.journal

---------------------------------------------------------------------
-- to Markdown

jsetsToMkd :: T.HasIssue a => T.Collection a -> Text
jsetsToMkd = Tx.unlines . map jsetToMkd . J.unpack

jsetToMkd :: T.HasIssue a => T.JournalSet a -> Text
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
                               , Vc.volIss iss
                               , ", "
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
-- Viewing tables of contents

---------------------------------------------------------------------
-- As Text

tocsToTxt :: T.JournalSet T.IssueContent -> Text
tocsToTxt (T.JSet _ cs) = Tx.intercalate "\n" . map tocToTxt $ cs

tocToTxt :: T.IssueContent -> Text
tocToTxt (T.IssueContent sel cs) = Tx.intercalate "\n"
                                 . (issueToTxt sel <> "\n" :)
                                 . map ( citationToTxt sel ) $ cs

---------------------------------------------------------------------
-- As Markdown

tocsToMkd :: T.JournalSet T.IssueContent -> Text
tocsToMkd (T.JSet setNo cs) = Tx.unlines . (:) hdr . map tocToMkd $ cs
    where hdr = "# Journal Set " <> C.tshow setNo

tocToMkd :: T.IssueContent -> Text
tocToMkd (T.IssueContent x cs)
    | null cs   = "## " <> issueToMkd x <> "\nNo citations listed at PubMed.\n"
    | otherwise = "## " <> xs
    where xs = Tx.unlines . (issueToMkd x :) . map (citationToMkd x) $ cs

---------------------------------------------------------------------
-- As HTML

tocsToHtml :: T.ToCStyle -> T.JournalSet T.IssueContent -> Text
tocsToHtml T.Propose jset = Html.htmlToCPropose jset
tocsToHtml T.Select  jset = Html.htmlToCSelect  jset
tocsToHtml T.Rank    jset = Html.htmlToCRank    jset

-- =============================================================== --
-- Formatting selection sets

---------------------------------------------------------------------
-- As text

selectionToTxt :: T.JournalSet T.Selection -> Text
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
