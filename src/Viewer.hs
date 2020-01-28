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
        where tbl  = Tx.unlines . map (tabulateSet keys) $ jsets
              hdr  = Tx.intercalate "," $ "No. & Date" : keys
              keys = map T.key js

tabulateSet :: [Text] -> T.JournalSet -> Text
tabulateSet keys jset = (hdr <>) . Tx.intercalate "," . foldr go [] $ keys
    where volIss  = bracket '\"' '\"' . Tx.intercalate "\n" . map viewVolIss
          go k xs = (volIss . J.issuesByKey k . snd) jset : xs
          date    = Tx.pack . show . J.dateOfSet $ jset
          hdr     = bracket '\"' '\"' (fst jset <> "\n" <> date) <> ","

viewSet :: T.JournalSet -> Text
viewSet jset = Tx.concat [fst jset, " | ", d, "\n"] <> Tx.unlines xs
    where xs    = map viewIssue . sortBy (comparing jName) . snd $ jset
          jName = T.name . T.journal
          d     = Tx.pack . show . J.dateOfSet $ jset

bracket :: Char -> Char -> Text -> Text
bracket x y = flip Tx.snoc y . Tx.cons x

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
