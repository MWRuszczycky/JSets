{-# LANGUAGE OverloadedStrings #-}

module Viewer
    ( viewIssue
    , viewJournalSet
    ) where

import qualified Data.Text   as Tx
import qualified Model.Types as T
import           Data.Text          (Text)

viewJournalSet :: [T.Issue] -> Text
viewJournalSet = Tx.unlines . map viewIssue

viewIssue :: T.Issue -> Text
viewIssue x = Tx.unwords us
    where us = [ T.abbr  . T.journal      $ x
               , Tx.pack . show . T.volNo $ x
               , Tx.pack . show . T.issNo $ x
               , Tx.pack $ "(" ++ show (T.date x) ++ ")"
               ]
