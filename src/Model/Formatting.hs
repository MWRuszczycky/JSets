{-# LANGUAGE OverloadedStrings #-}

module Model.Formatting
    ( -- Classes
      Formattable (..)
      -- Formatting journal sets
      -- As CSV
    , jsetsToCsv
    , jsetToCsv
      -- As Text
    , jsetsToTxt
    , jsetToTxt
    , jsetKeyToTxt
      -- Formatting Issues
      -- As Text
    , issueToTxt
    , issueToTxtVerbose
    , volIssToTxt
      -- Formatting tables of contents
      -- As Text
    , tocsToTxt
    , tocToTxt
    , citationToTxt
      -- As Markdown
    , tocsToMkd
    , tocToMkd
    , citationToMkd
      -- Formatting helper functions
    , bracket
    ) where

import qualified Data.Text          as Tx
import qualified Model.Core.Core    as C
import qualified Model.Core.Types   as T
import qualified Model.Journals     as J
import           Data.Text                  ( Text      )
import           Data.List                  ( sortBy    )
import           Data.Ord                   ( comparing )

-- =============================================================== --
-- Classes

class Formattable a where
    format :: T.Format -> [Text] -> a -> Text

-- =============================================================== --
-- Formatting journal sets

instance Formattable T.JournalSets where
    format T.CSV hdr jsets = jsetsToCsv hdr jsets
    format T.TXT hdr jsets = jsetsToTxt hdr jsets
    format T.MKD _   _     = "Collections not formattable as Markdown."

instance Formattable T.JournalSet where
    format T.CSV hdr jset = jsetToCsv hdr jset
    format T.TXT hdr jset = jsetToTxt hdr jset
    format T.MKD _   _    = "Journal sets not formattable as Markdown"

---------------------------------------------------------------------
-- As CSV

jsetsToCsv :: [Text] -> T.JournalSets -> Text
-- ^Convert a list of journal sets to CSV. Every element is enclosed
-- in double quotes. The first line is the list of journals by
-- journal key as specified by the first argument. Every subsequent
-- line is a journal set with issues for a given journal separated by
-- new line characters.
jsetsToCsv keys jsets = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (jsetToCsv keys) . J.unpack $ jsets
              hdr  = Tx.intercalate "," $ "No. & Date" : keys

jsetToCsv :: [Text] -> T.JournalSet -> Text
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journals in the order they will be tabulated. The
-- second argument is the journal set. The first cell will be the
-- key associated with the journal set. Subsequent cells will list
-- the issues for the corresponding journal separated by newline
-- characters. All elements are enclosed in double quotes.
jsetToCsv keys jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ keys
    where go k xs = (volIss . J.issuesByKey k . T.jsIssues) jset : xs
          volIss  = bracket '\"' '\"' . Tx.intercalate "\n" . map volIssToTxt
          hdr     = bracket '\"' '\"' (keyStr <> "\n" <> date) <> ","
          date    = C.tshow . J.dateOfJSet $ jset
          keyStr  = C.tshow . T.jsKey $ jset

---------------------------------------------------------------------
-- As Text

jsetsToTxt :: [Text] -> T.JournalSets -> Text
jsetsToTxt hdr jsets = Tx.unlines . map (jsetToTxt hdr) . J.unpack $ jsets

jsetToTxt :: [Text] -> T.JournalSet -> Text
-- ^Convert a journal set to easily readable, formatted text.
jsetToTxt _ js = jsetKeyToTxt js <> "\n" <> Tx.unlines xs
    where xs    = map issueToTxt . sortBy (comparing jName) . T.jsIssues $ js
          jName = T.name . T.journal

jsetKeyToTxt :: T.JournalSet -> Text
-- ^Convert a journal set key to text formatted as year-number.
jsetKeyToTxt js = Tx.unwords [ (C.tshow . T.jsKey) js
                             , "|"
                             , C.tshow . J.dateOfJSet $ js
                             ]

-- =============================================================== --
-- Formatting issues

---------------------------------------------------------------------
-- As Text

issueToTxt :: T.Issue -> Text
-- ^Convert a journal issue to easily readable, formatted text.
issueToTxt x = Tx.unwords us
    where us = [ T.key . T.journal $ x
               , C.tshow . T.volNo $ x
               , C.tshow . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]

issueToTxtVerbose :: T.Issue -> Text
issueToTxtVerbose = issueToTxt

volIssToTxt :: T.Issue -> Text
-- ^Construct a vol:iss text string for the volume and issue of a
-- journal issue.
volIssToTxt x = Tx.intercalate ":" volIss
    where volIss = map C.tshow [ T.volNo x, T.issNo x ]

-- =============================================================== --
-- Formatting tables of contents

instance Formattable T.JournalSetToC where
    format T.CSV _ _ = "Tables of contents not formattable as CSV"
    format T.TXT hdr toc = tocsToTxt hdr toc
    format T.MKD hdr toc = tocsToMkd hdr toc

---------------------------------------------------------------------
-- As Text

tocsToTxt :: [Text] -> T.JournalSetToC -> Text
tocsToTxt _ (T.JSetToC _ tocs) = Tx.unlines . map tocToTxt $ tocs

tocToTxt :: T.IssueToC -> Text
tocToTxt (T.IssueToC x cs) = Tx.unlines
                             . (issueToTxt x <> "\n" :)
                             . map (citationToTxt x) $ cs

pagesToTxt :: T.Citation -> Text
pagesToTxt x = C.tshow p1 <> "-" <> C.tshow p2
    where (p1,p2) = T.pages x

citationToTxt :: T.Issue -> T.Citation -> Text
citationToTxt iss x = Tx.unlines parts
    where jrnl  = T.journal iss
          parts = [ T.title x
                  , T.authors x
                  , Tx.unwords [ T.name jrnl,  volIssToTxt iss, pagesToTxt x ]
                  ]

---------------------------------------------------------------------
-- As Markdown

tocsToMkd :: [Text] -> T.JournalSetToC -> Text
tocsToMkd _ ( T.JSetToC k ts ) = Tx.unlines . (:) hdr . map tocToMkd $ ts
    where hdr = "# Journal Set " <> C.tshow k

tocToMkd :: T.IssueToC -> Text
tocToMkd (T.IssueToC x []) = issueToMkdHeader x
tocToMkd (T.IssueToC x cs) = Tx.unlines
                             . (issueToMkdHeader x :)
                             . map (citationToMkd x) $ cs

issueToMkdHeader :: T.Issue -> Text
issueToMkdHeader iss = Tx.unwords [ "##"
                                  , T.name . T.journal $ iss
                                  , volIssToTxt iss <> "\n"
                                  ]

citationToMkd :: T.Issue -> T.Citation -> Text
citationToMkd iss x = Tx.unlines parts
    where jrnl  = T.journal iss
          parts = [ (mkdBd $ mkdLink (fixMkd $ T.title x) (T.doi x)) <> "\\"
                  , fixMkd (T.authors x) <> "\\"
                  , Tx.unwords [ mkdIt . fixMkd . T.name $ jrnl
                               , volIssToTxt iss
                               , pagesToTxt x
                               ]
                  ]

-- =============================================================== --
-- Formatting selection sets

instance Formattable T.SelectionSet where
    format T.CSV _   _   = "Selections not formattable as CSV"
    format T.TXT hdr sel = selectionToText hdr sel
    format T.MKD _   _   = "Selections not formattable as MKD"

---------------------------------------------------------------------
-- As text

selectionToText :: [Text] -> T.SelectionSet -> Text
selectionToText _ sel = Tx.unlines $ jsetKeyToTxt jset : concatMap go xs
    where jset      = T.JSet (T.selKey sel) (fst . unzip $ xs)
          xs        = T.selIssues sel
          go (x,ps) = issueToTxt x : map ( \ n -> "    " <> C.tshow n ) ps

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
