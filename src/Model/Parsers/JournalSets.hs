{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJsets
    ) where

import qualified Data.Text             as Tx
import qualified Model.Core.Types      as T
import qualified Model.Core.References as R
import qualified Model.Parsers.CSV     as CSV
import qualified Model.Journals        as J
import qualified Data.Map.Strict       as Map
import           Data.Text                    ( Text         )
import           Data.Maybe                   ( listToMaybe  )
import           Model.Core.Core              ( readMaybeTxt )

-- =============================================================== --
-- Main parser

parseJsets :: Text -> Either String T.JournalSets
-- ^Parse a properly formatted csv file to JournalSets.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
parseJsets x = CSV.parseCSV x >>= toJournalSets

toJournalSets :: [[Text]] -> Either String T.JournalSets
-- ^Convert a parsed CSV file to Journal Sets.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
toJournalSets []     = pure Map.empty
toJournalSets (x:xs) = do
    jKeys <- toJournalKeys x
    jSets <- traverse (toJournalSet jKeys) xs
    if duplicateKeys jSets
       then Left "There duplicated journal set keys."
       else pure . Map.fromList $ jSets

-- =============================================================== --
-- Component parsers

---------------------------------------------------------------------
-- Journal keys

toJournalKeys :: [Text] -> Either String [Text]
-- ^The first row in the csv file is the journal abbreviations. The
-- first element is a dummy header for the journal set keys, so it
-- needs to be dropped. Each journal must have a reference issue
-- available for this to succeed.
toJournalKeys []     = Left "Nothing to parse."
toJournalKeys (_:[]) = Left "Nothing to parse."
toJournalKeys (_:ks) = traverse go ks
    where errMsg x = "Journal key unavailable: " ++ Tx.unpack x
          go x | R.isAvailable x = pure x
               | otherwise       = Left . errMsg $ x

---------------------------------------------------------------------
-- Individual journal sets and issues

toJournalSet :: [Text] -> [Text] -> Either String T.JournalSet
-- ^Convert all csv cell contents as Text values to a journal set.
-- The journal set must begin with a correctly formatted key.
toJournalSet _  []     = Left "Missing key for journal set."
toJournalSet js (x:xs) = (,) <$> parseKey x <*> toIssuesInSet js xs

parseKey :: Text -> Either String (Int, Int)
-- ^Parse the journal set key. The journal set key must be of the
-- form 'Y-N, where Y is the four digit year and N is the set number.
-- These are then converted to the (Int, Int) key used to index the
-- journal sets.
parseKey t = maybe err pure yn
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t
          yn  = case fmap (Tx.splitOn "-") . listToMaybe . Tx.lines $ t of
                     Just (y:n:_) -> (,) <$> readMaybeTxt y <*> readMaybeTxt n
                     _            -> Nothing

toIssuesInSet :: [Text] -> [Text] -> Either String [T.Issue]
-- ^Generate the issues for each journal in a csv line corresponding
-- to a single journal set. The first argument is the list of
-- journals. The second argument is the volume and issue numbers for
-- the corresponding journal in the same order. All volume and issue
-- numbers must correspond to valid issues for the journal.
toIssuesInSet js xs = fmap concat . traverse toIssues . zip js $ ys
    where ys = xs ++ replicate (length js - length xs) Tx.empty

toIssues :: (Text, Text) -> Either String [T.Issue]
-- ^Given a journal key (abbreviation) and Text corresponding to
-- volume and issue numbers for that journal, construct a
-- corresponding issue if it exists. The volume and issue numbers
-- must be formatted as 'volume:issue'. Multiple issues can be
-- specified by separating the 'volume:issue' text by newlines.
toIssues (j, t) = mapM toVolIssNo xs >>= mapM (go . J.lookupIssue j)
    where xs          = Tx.lines t
          go (Just x) = pure x
          go Nothing  = Left . Tx.unpack . Tx.unwords $ [ "Missing Issue(s):"
                                                        , j, Tx.unwords xs
                                                        ]

-- =============================================================== --
-- Helper functions

duplicateKeys :: [T.JournalSet] -> Bool
-- ^Check for duplicated journal set keys.
duplicateKeys = go . fst . unzip
    where go []     = False
          go (x:xs) = elem x xs || go xs

toVolIssNo :: Text -> Either String (Int, Int)
-- ^Parse a 'volume:issue' text string to the numeric values.
toVolIssNo t = case traverse readMaybeTxt . Tx.splitOn ":" $ t of
                    Just (v:i:[]) -> pure (v,i)
                    _             -> Left "Cannot parse volume:issue"
