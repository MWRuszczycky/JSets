{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Formatting
    ( -- Formatting journal sets
      -- As CSV
      jsetsToCsv
    , jsetToCsv
      -- As Text
    , jsetsToTxt
    , jsetToTxt
    , jsetHeader
      -- Formatting Issues
      -- As Text
    , issueToTxt
    , volIssToTxt
      -- Formatting tables of contents
      -- As Text
    , tocsToTxt
    , tocToTxt
    , citationToTxt
      -- As html
    , tocsToHtml
      -- As Markdown
    , tocsToMkd
    , tocToMkd
    , citationToMkd
      -- Formatting selections
    , selectionToTxt
      -- Fomatting references
    , referenceToTxt
    ) where

import qualified Data.Text            as Tx
import qualified Model.Core.Core      as C
import qualified Model.Core.Types     as T
import qualified Model.Journals       as J
import qualified Model.Text.Html      as Html
import           Data.Text                      ( Text      )
import           Data.List                      ( sortBy    )
import           Data.Ord                       ( comparing )

-- =============================================================== --
-- Formatting journal sets

---------------------------------------------------------------------
-- As CSV

jsetsToCsv :: [Text] -> T.Collection -> Text
-- ^Convert a collection of journal sets to CSV. Every element is
-- enclosed in double quotes. The first line is the list of journals
-- by journal abbreviation as specified by the first argument. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsToCsv abbrs col = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (jsetToCsv abbrs) . J.unpack $ col
              hdr  = Tx.intercalate "," $ "No. & Date" : abbrs

jsetToCsv :: T.IsIssue a => [Text] -> T.JournalSet a -> Text
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journal abbreviations in the order they will be
-- tabulated. The second argument is the journal set. The first cell
-- will be the key associated with the journal set. Subsequent cells
-- will list the issues for the corresponding journal separated by
-- newline characters. All elements are enclosed in double quotes.
jsetToCsv abbrs jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ abbrs
    where go k xs = (volIss . J.issuesByAbbr k . T.issues) jset : xs
          volIss  = bracket '\"' '\"' . Tx.intercalate "\n" . map volIssToTxt
          hdr     = bracket '\"' '\"' ( setNo <> "\n" <> date ) <> ","
          date    = C.tshow . J.dateOfJSet $ jset
          setNo   = C.tshow . T.setNo $ jset

---------------------------------------------------------------------
-- As Text

jsetsToTxt :: T.Collection -> Text
jsetsToTxt = Tx.unlines . map jsetToTxt . J.unpack

jsetToTxt :: T.IsIssue a => T.JournalSet a -> Text
-- ^Convert a journal set to easily readable, formatted text.
jsetToTxt jset = jsetHeader jset <> "\n" <> Tx.unlines xs
    where xs    = map issueToTxt . sortBy (comparing jName) . T.issues $ jset
          jName = T.name . T.journal

---------------------------------------------------------------------
-- Helpers

jsetHeader :: T.IsIssue a => T.JournalSet a -> Text
-- ^Convert a journal set key to text formatted as year-number.
jsetHeader jset = Tx.unwords [ (C.tshow . T.setNo) jset
                             , "|"
                             , C.tshow . J.dateOfJSet $ jset
                             ]

-- =============================================================== --
-- Formatting issues

---------------------------------------------------------------------
-- As Text

issueToTxt :: T.IsIssue a => a -> Text
-- ^Convert a journal issue to easily readable, formatted text.
issueToTxt iss = Tx.unwords us
    where us = [ T.abbr . T.journal $ iss
               , C.tshow . T.volNo  $ iss
               , C.tshow . T.issNo  $ iss
               , Tx.pack $ "(" ++ show (T.date iss) ++ ")"
               ]

volIssToTxt :: T.IsIssue a => a -> Text
-- ^Construct a vol:iss text string for the volume and issue of a
-- journal issue.
volIssToTxt iss = Tx.intercalate ":" volIss
    where volIss = map C.tshow [ T.volNo iss, T.issNo iss ]

-- =============================================================== --
-- Formatting tables of contents

---------------------------------------------------------------------
-- As Text

tocsToTxt :: T.JournalSet T.CitedIssue -> Text
tocsToTxt (T.JSet _ iss) = Tx.unlines . map tocToTxt $ iss

tocToTxt :: T.CitedIssue -> Text
tocToTxt (T.CitedIssue iss cs) = Tx.unlines
                                 . (issueToTxt iss <> "\n" :)
                                 . map ( citationToTxt iss ) $ cs

pagesToTxt :: T.Citation -> Text
pagesToTxt x = C.tshow p1 <> "-" <> C.tshow p2
    where (p1,p2) = T.pages x

citationToTxt :: T.IsIssue a => a -> T.Citation -> Text
citationToTxt iss c = Tx.unlines parts
    where jrnl  = T.journal iss
          parts = [ T.title c
                  , T.authors c
                  , Tx.unwords [ T.name jrnl,  volIssToTxt iss, pagesToTxt c ]
                  ]

---------------------------------------------------------------------
-- As Markdown

tocsToMkd :: T.JournalSet T.CitedIssue -> Text
tocsToMkd (T.JSet setNo cs) = Tx.unlines . (:) hdr . map tocToMkd $ cs
    where hdr = "# Journal Set " <> C.tshow setNo

tocToMkd :: T.CitedIssue -> Text
tocToMkd (T.CitedIssue x []) = issueToMkdHeader x
tocToMkd (T.CitedIssue x cs) = Tx.unlines
                               . (issueToMkdHeader x :)
                               . map (citationToMkd x) $ cs

issueToMkdHeader :: T.IsIssue a => a -> Text
issueToMkdHeader iss = Tx.unwords [ "##"
                                  , T.name . T.journal $ iss
                                  , volIssToTxt iss <> "\n"
                                  ]

citationToMkd :: T.SelIssue -> T.Citation -> Text
citationToMkd iss x = Tx.unlines parts
    where jrnl  = T.journal iss
          parts = [ (mkdBd $ mkdLink (fixMkd $ T.title x) (T.doi x)) <> "\\"
                  , fixMkd (T.authors x) <> "\\"
                  , Tx.unwords [ mkdIt . fixMkd . T.name $ jrnl
                               , volIssToTxt iss
                               , pagesToTxt x
                               ]
                  ]

---------------------------------------------------------------------
-- As HTML

tocsToHtml :: T.ToCStyle -> T.JournalSet T.CitedIssue -> Text
tocsToHtml T.Propose jset = Html.htmlToCPropose jset
tocsToHtml T.Select  jset = Html.htmlToCSelect  jset
tocsToHtml T.Rank    jset = Html.htmlToCRank    jset

-- =============================================================== --
-- Formatting selection sets

---------------------------------------------------------------------
-- As text

selectionToTxt :: T.JournalSet T.SelIssue -> Text
selectionToTxt jset@(T.JSet _ xs) =
    let go x = issueToTxt x : map ( \ n -> "    " <> C.tshow n ) (T.selection x)
    in  Tx.unlines $ jsetHeader jset : concatMap go xs

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

-- =============================================================== --
-- Helper functions

---------------------------------------------------------------------
-- General formatting

bracket :: Char -> Char -> Text -> Text
-- ^Place characters at the front and back of a text string.
bracket x y = flip Tx.snoc y . Tx.cons x

---------------------------------------------------------------------
-- Markdown formatting

fixMkd :: Text -> Text
-- ^Make any corrections to general text that may interfere with
-- Markdown formatting:
--  1. Change brackets to avoid inteference with url links.
fixMkd = Tx.map go
    where go x | x == '['  = toEnum 0x27e6
               | x == ']'  = toEnum 0x27e7
               | otherwise = x

mkdBd :: Text -> Text
-- ^Make text bold in Markdown.
mkdBd x = "**" <> x <> "**"

mkdIt :: Text -> Text
-- ^Make text itallic in Markdown.
mkdIt = bracket '*' '*'

mkdLink :: Text -> Text -> Text
-- ^Add a url link in Markdown.
mkdLink content url = contentMkd <> link
    where contentMkd = bracket '[' ']' content
          link       = bracket '(' ')' url
