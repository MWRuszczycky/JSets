{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( viewIssue
    , tabulateJSet
    , tabulateJSets
    , viewJSet
    ) where

import qualified Data.Text      as Tx
import qualified Data.Map       as Map
import qualified Model.Types    as T
import qualified Model.Journals as J
import qualified Model.Core     as C
import           Data.Text              ( Text      )
import           Data.List              ( sortBy    )
import           Data.Ord               ( comparing )

-- =============================================================== --
-- Converting journal sets to text strings

tabulateJSets :: [T.Journal] -> T.JournalSets -> Text
tabulateJSets js jsets = hdr <> "\n" <> tbl
        where tbl  = Tx.unlines . map (tabulateJSet keys) . Map.toList $ jsets
              hdr  = Tx.intercalate "," $ "No. & Date" : keys
              keys = map T.key js

tabulateJSet :: [Text] -> T.JournalSet -> Text
tabulateJSet keys jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ keys
    where volIss  = bracket '\"' '\"' . Tx.intercalate "\n" . map viewVolIss
          go k xs = (volIss . J.issuesByKey k . snd) jset : xs
          date    = Tx.pack . show . J.dateOfJSet $ jset
          hdr     = bracket '\"' '\"' (viewJSetKey jset <> "\n" <> date) <> ","

viewJSet :: T.JournalSet -> Text
viewJSet jset = Tx.concat [viewJSetKey jset, " | ", d, "\n"] <> Tx.unlines xs
    where xs    = map viewIssue . sortBy (comparing jName) . snd $ jset
          jName = T.name . T.journal
          d     = Tx.pack . show . J.dateOfJSet $ jset

viewJSetKey :: T.JournalSet -> Text
viewJSetKey ((y,n),_) = C.txt y <> "-" <> C.txt n

-- =============================================================== --
-- Converting issues to text strings

viewVolIss :: T.Issue -> Text
viewVolIss x = Tx.intercalate ":" volIss
    where volIss = map (Tx.pack . show) [ T.volNo x, T.issNo x ]

viewIssue :: T.Issue -> Text
viewIssue x = Tx.unwords us
    where us = [ T.key . T.journal        $ x
               , Tx.pack . show . T.volNo $ x
               , Tx.pack . show . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]

-- =============================================================== --
-- Helper functions

bracket :: Char -> Char -> Text -> Text
-- ^Place characters at the front and back of a text string.
bracket x y = flip Tx.snoc y . Tx.cons x
