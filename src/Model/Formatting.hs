{-# LANGUAGE OverloadedStrings #-}

module Model.Formatting
    ( -- Converting journal sets to CSV
      jsetsToCSV
    , jsetToCSV
      -- Converting journal sets to text
    , jsetsToTxt
    , jsetToTxt
    , jsetKeyToTxt
      -- Converting issues to text strings
    , issueToTxt
    , volIssToTxt
      -- Viewing table of contents
    , tocToTxt
    , citationToTxt
    , tocsToMkd
    , tocToMkd
    , citationToMkd
      -- Formatting helper functions
    , bracket
    ) where

import qualified Data.Text          as Tx
import qualified Data.Map           as Map
import qualified Model.Core.Core    as C
import qualified Model.Core.Types   as T
import qualified Model.Journals     as J
import           Data.Text                  ( Text      )
import           Data.List                  ( sortBy    )
import           Data.Ord                   ( comparing )

-- =============================================================== --
-- Converting journal sets to CSV

jsetsToCSV :: [T.Journal] -> T.JournalSets -> Text
-- ^Convert a list of journal sets to CSV. Every element is enclosed
-- in double quotes. The first line is the list of journals. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jsetsToCSV js jsets = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (jsetToCSV keys) . Map.toList $ jsets
              hdr  = Tx.intercalate "," $ "No. & Date" : keys
              keys = map T.key js

jsetToCSV :: [Text] -> T.JournalSet -> Text
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journals in the order they will be tabulated. The
-- second argument is the journal set. The first cell will be the
-- key associated with the journal set. Subsequent cells will list
-- the issues for the corresponding journal separated by newline
-- characters. All elements are enclosed in double quotes.
jsetToCSV keys jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ keys
    where go k xs = (volIss . J.issuesByKey k . snd) jset : xs
          volIss  = bracket '\"' '\"' . Tx.intercalate "\n" . map volIssToTxt
          hdr     = bracket '\"' '\"' (jsetKeyToTxt jset <> "\n" <> date) <> ","
          date    = C.tshow . J.dateOfJSet $ jset

-- =============================================================== --
-- Converting journal sets to text

jsetsToTxt :: T.JournalSets -> Text
jsetsToTxt = Tx.unlines . map jsetToTxt . Map.toList

jsetToTxt :: T.JournalSet -> Text
-- ^Convert a journal set to easily readable, formatted text.
jsetToTxt jset = Tx.concat [jsetKeyToTxt jset, " | ", d, "\n"] <> Tx.unlines xs
    where xs    = map issueToTxt . sortBy (comparing jName) . snd $ jset
          jName = T.name . T.journal
          d     = C.tshow . J.dateOfJSet $ jset

jsetKeyToTxt :: T.JournalSet -> Text
-- ^Convert a journal set key to text formatted as year-number.
jsetKeyToTxt ((y,n),_) = C.tshow y <> "-" <> C.tshow n

-- =============================================================== --
-- Converting issues to text strings

issueToTxt :: T.Issue -> Text
-- ^Convert a journal issue to easily readable, formatted text.
issueToTxt x = Tx.unwords us
    where us = [ T.key . T.journal        $ x
               , C.tshow . T.volNo $ x
               , C.tshow . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]

volIssToTxt :: T.Issue -> Text
-- ^Construct a vol:iss text string for the volume and issue of a
-- journal issue.
volIssToTxt x = Tx.intercalate ":" volIss
    where volIss = map C.tshow [ T.volNo x, T.issNo x ]

-- =============================================================== --
-- Viewing table of contents

---------------------------------------------------------------------
-- General viewing

tocToTxt :: T.TableOfContents -> Text
tocToTxt = Tx.unlines . map citationToTxt

pagesToTxt :: T.Citation -> Text
pagesToTxt x = C.tshow p1 <> "-" <> C.tshow p2
    where (p1,p2) = T.pages x

citationToTxt :: T.Citation -> Text
citationToTxt x = Tx.unlines parts
    where iss   = T.issue x
          jrnl  = T.journal iss
          parts = [ T.title x
                  , T.authors x
                  , Tx.unwords [ T.name jrnl,  volIssToTxt iss, pagesToTxt x ]
                  ]

---------------------------------------------------------------------
-- Conversion to Markdown

tocsToMkd :: (Int,Int) -> [T.TableOfContents] -> Text
tocsToMkd (y,n) = Tx.unlines . (:) hdr . map tocToMkd
    where hdr = "# Journal Set " <> C.tshow y <> "-" <> C.tshow n

tocToMkd :: T.TableOfContents -> Text
tocToMkd []       = Tx.empty
tocToMkd xs@(x:_) = Tx.unlines . (hdr:) . map citationToMkd $ xs
    where iss = T.issue x
          hdr = Tx.unwords [ "##"
                           , T.name . T.journal $ iss
                           , volIssToTxt iss <> "\n"
                           ]

citationToMkd :: T.Citation -> Text
citationToMkd x = Tx.unlines parts
    where iss   = T.issue x
          jrnl  = T.journal iss
          parts = [ (mkdBd $ mkdLink (fixMkd $ T.title x) (T.doi x)) <> "\\"
                  , fixMkd (T.authors x) <> "\\"
                  , Tx.unwords [ mkdIt . fixMkd . T.name $ jrnl
                               , volIssToTxt iss
                               , pagesToTxt x
                               ]
                  ]

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
