{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( viewIssue
    , tabulateSet
    , tabulateSets
    , viewSet
    ) where

import qualified Data.Text      as Tx
import qualified Model.Types    as T
import qualified Model.Journals as J
import           Data.Text              ( Text      )
import           Data.List              ( sortBy    )
import           Data.Ord               ( comparing )

tabulateSets :: [T.Journal] -> [T.JournalSet] -> Text
tabulateSets js jsets = hdr <> "\n" <> tbl
        where tbl = Tx.unlines . map (tabulateSet nms) $ jsets
              hdr = Tx.intercalate "," $ "No. & Date" : abs
              nms = map T.name js
              abs = map T.abbr js

tabulateSet :: [Text] -> T.JournalSet -> Text
tabulateSet ns jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ ns
    where fmt     = bracket '\"' '\"' . Tx.intercalate "\n" . map volIss
          go n xs = (fmt . issuesInSet n) jset : xs
          d       = Tx.pack . show . J.dateOfSet $ jset
          hdr     = flip Tx.snoc ',' . bracket '\"' '\"' $ fst jset <> "\n" <> d

viewSet :: T.JournalSet -> Text
viewSet jset = Tx.concat [fst jset, " | ", d, "\n"] <> Tx.unlines xs
    where xs    = map viewIssue . sortBy (comparing jName) . snd $ jset
          jName = T.name . T.journal
          d     = Tx.pack . show . J.dateOfSet $ jset

bracket :: Char -> Char -> Text -> Text
bracket x y = flip Tx.snoc y . Tx.cons x

issuesInSet :: Text -> T.JournalSet -> [T.Issue]
issuesInSet name = filter ( (== name) . T.name . T.journal ) . snd

volIss :: T.Issue -> Text
volIss x = Tx.intercalate ":" . map (Tx.pack . show) $ [ T.volNo x, T.issNo x ]

viewIssue :: T.Issue -> Text
viewIssue x = Tx.unwords us
    where us = [ T.abbr  . T.journal      $ x
               , Tx.pack . show . T.volNo $ x
               , Tx.pack . show . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]
