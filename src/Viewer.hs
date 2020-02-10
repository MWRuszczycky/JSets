{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( -- Converting journal sets to CSV
      jSetsToCSV
    , jSetToCSV
      -- Converting journal sets to text
    , viewJSet
    , viewJSetKey
      -- Converting issues to text strings
    , viewIssue
    , viewVolIss
      -- Viewing table of contents
    , viewToC
    , viewCitation
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

jSetsToCSV :: [T.Journal] -> T.JournalSets -> Text
-- ^Convert a list of journal sets to CSV. Every element is enclosed
-- in double quotes. The first line is the list of journals. Every
-- subsequent line is a journal set with issues for a given journal
-- separated by new line characters.
jSetsToCSV js jsets = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (jSetToCSV keys) . Map.toList $ jsets
              hdr  = Tx.intercalate "," $ "No. & Date" : keys
              keys = map T.key js

jSetToCSV :: [Text] -> T.JournalSet -> Text
-- ^Convert a journal set to a single line of CSV. The first argument
-- is the list of journals in the order they will be tabulated. The
-- second argument is the journal set. The first cell will be the
-- key associated with the journal set. Subsequent cells will list
-- the issues for the corresponding journal separated by newline
-- characters. All elements are enclosed in double quotes.
jSetToCSV keys jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ keys
    where go k xs = (volIss . J.issuesByKey k . snd) jset : xs
          volIss  = bracket '\"' '\"' . Tx.intercalate "\n" . map viewVolIss
          hdr     = bracket '\"' '\"' (viewJSetKey jset <> "\n" <> date) <> ","
          date    = C.tshow . J.dateOfJSet $ jset

-- =============================================================== --
-- Converting journal sets to text

viewJSet :: T.JournalSet -> Text
-- ^Convert a journal set to easily readable, formatted text.
viewJSet jset = Tx.concat [viewJSetKey jset, " | ", d, "\n"] <> Tx.unlines xs
    where xs    = map viewIssue . sortBy (comparing jName) . snd $ jset
          jName = T.name . T.journal
          d     = C.tshow . J.dateOfJSet $ jset

viewJSetKey :: T.JournalSet -> Text
-- ^Convert a journal set key to text formatted as year-number.
viewJSetKey ((y,n),_) = C.tshow y <> "-" <> C.tshow n

-- =============================================================== --
-- Converting issues to text strings

viewIssue :: T.Issue -> Text
-- ^Convert a journal issue to easily readable, formatted text.
viewIssue x = Tx.unwords us
    where us = [ T.key . T.journal        $ x
               , C.tshow . T.volNo $ x
               , C.tshow . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]

viewVolIss :: T.Issue -> Text
-- ^Construct a vol:iss text string for the volume and issue of a
-- journal issue.
viewVolIss x = Tx.intercalate ":" volIss
    where volIss = map C.tshow [ T.volNo x, T.issNo x ]

-- =============================================================== --
-- Viewing table of contents

---------------------------------------------------------------------
-- General viewing

viewToC :: T.TableOfContents -> Text
viewToC = Tx.unlines . map viewCitation

viewPages :: T.Citation -> Text
viewPages x = C.tshow p1 <> "-" <> C.tshow p2
    where (p1,p2) = T.pages x

viewCitation :: T.Citation -> Text
viewCitation x = Tx.unlines parts
    where iss   = T.issue x
          jrnl  = T.journal iss
          parts = [ T.title x
                  , T.authors x
                  , Tx.unwords [ T.name jrnl,  viewVolIss iss, viewPages x ]
                  ]

---------------------------------------------------------------------
-- Conversion to Markdown

tocToMkd :: T.TableOfContents -> Text
tocToMkd []       = Tx.empty
tocToMkd xs@(x:_) = Tx.unlines . (hdr:) . map citationToMkd $ xs
    where hdr = flip Tx.snoc '\n' . (<>) "## "
                . T.name . T.journal . T.issue $ x

citationToMkd :: T.Citation -> Text
citationToMkd x = Tx.unlines parts
    where iss   = T.issue x
          jrnl  = T.journal iss
          parts = [ mkdBd ( mkdLink (T.title x) (T.doi x) ) <> "\\"
                  , T.authors x <> "\\"
                  , Tx.unwords [ mkdIt $ T.name jrnl
                               , viewVolIss iss
                               , viewPages x
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
