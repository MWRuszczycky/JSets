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
          date    = C.txt . J.dateOfJSet $ jset

-- =============================================================== --
-- Converting journal sets to text

viewJSet :: T.JournalSet -> Text
-- ^Convert a journal set to easily readable, formatted text.
viewJSet jset = Tx.concat [viewJSetKey jset, " | ", d, "\n"] <> Tx.unlines xs
    where xs    = map viewIssue . sortBy (comparing jName) . snd $ jset
          jName = T.name . T.journal
          d     = C.txt . J.dateOfJSet $ jset

viewJSetKey :: T.JournalSet -> Text
-- ^Convert a journal set key to text formatted as year-number.
viewJSetKey ((y,n),_) = C.txt y <> "-" <> C.txt n

-- =============================================================== --
-- Converting issues to text strings

viewIssue :: T.Issue -> Text
-- ^Convert a journal issue to easily readable, formatted text.
viewIssue x = Tx.unwords us
    where us = [ T.key . T.journal        $ x
               , C.txt . T.volNo $ x
               , C.txt . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]

viewVolIss :: T.Issue -> Text
-- ^Construct a vol:iss text string for the volume and issue of a
-- journal issue.
viewVolIss x = Tx.intercalate ":" volIss
    where volIss = map C.txt [ T.volNo x, T.issNo x ]

-- =============================================================== --
-- Viewing table of contents

viewToC :: T.TableOfContents -> Text
viewToC = C.txt

-- =============================================================== --
-- Helper functions

bracket :: Char -> Char -> Text -> Text
-- ^Place characters at the front and back of a text string.
bracket x y = flip Tx.snoc y . Tx.cons x
