{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJsetsCsv
    , parseJsetsTxt
    , parseJsets
    ) where

import qualified Data.Text             as Tx
import qualified Data.Attoparsec.Text  as At
import qualified Model.Core.Types      as T
import qualified Model.Core.References as R
import qualified Model.Parsers.CSV     as CSV
import qualified Model.Journals        as J
import           Data.Bifunctor               ( bimap              )
import           Data.Char                    ( isSpace, isDigit   )
import           Data.Text                    ( Text               )
import           Model.Core.Core              ( readMaybeTxt       )
import           Control.Applicative          ( some, many,  (<|>) )

-- =============================================================== --
-- Main parsers

parseJsets :: Text -> Either T.ErrString T.JournalSets
parseJsets x = parseJsetsCsv x <|> parseJsetsTxt x

parseJsetsCsv :: Text -> Either T.ErrString T.JournalSets
-- ^Parse a properly formatted csv file to JournalSets.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
-- All issues must be valid and the first row must be the journals.
parseJsetsCsv x = CSV.parseCSV x >>= toJournalSets

parseJsetsTxt :: Text -> Either T.ErrString T.JournalSets
-- ^Parse a properly formatted text file to JournalSets.
-- All issues must be valid on lookup.
parseJsetsTxt t = bimap err id $ At.parseOnly jsetsTxtParser t >>= validate
    where err   = (<>) "Cannot parse TXT: "

-- =============================================================== --
-- Local types

type RawIssue = ( Text, Int, Int  )

type RawJSet  = ( Int, [RawIssue] )

-- =============================================================== --
-- Parse validation

validate :: [RawJSet] -> Either T.ErrString T.JournalSets
validate js = mapM go js >>= packJSets
    where go (key, iss) = T.JSet key <$> mapM validateIssue iss

validateIssue :: RawIssue -> Either T.ErrString T.Issue
validateIssue (j,v,n) = maybe err pure . J.lookupIssue j $ (v,n)
    where err = Left $ invalidIssErr j v n

packJSets :: [T.JournalSet] -> Either T.ErrString T.JournalSets
packJSets js
    | duplicateKeys js = Left "There are duplicated journal set keys."
    | otherwise        = pure . J.pack $ js

-- =============================================================== --
-- Component parsers for TXT

jsetsTxtParser :: At.Parser [RawJSet]
jsetsTxtParser = At.skipSpace *> many jsetTxtParser <* At.endOfInput

jsetTxtParser :: At.Parser RawJSet
jsetTxtParser = do
    key <- jsetKeyParser
    iss <- many issueTxtParser
    At.skipSpace
    pure (key, iss)

jsetKeyParser :: At.Parser Int
jsetKeyParser = do
    key <- intParser
    At.skipSpace *> At.char '|' *> At.skipSpace
    dateParser *> At.skipSpace
    pure key

issueTxtParser :: At.Parser RawIssue
issueTxtParser = do
    journal <- Tx.init <$> At.takeWhile1 ( not . isDigit )
    volNo   <- intParser <* At.skipSpace
    issNo   <- intParser <* At.skipSpace
    At.char '(' *> dateParser *> At.char ')'
    At.skipSpace
    pure (journal, volNo, issNo)

intParser :: At.Parser Int
intParser = some At.digit >>= pure . read

dateParser :: At.Parser (Int, Int, Int)
dateParser = (,,) <$> ( intParser <* At.char '-'  )
                  <*> ( intParser <* At.char '-'  )
                  <*> ( intParser                 )

-- =============================================================== --
-- Component parsers for CSV

toJournalSets :: [[Text]] -> Either String T.JournalSets
-- ^Convert a parsed CSV file to Journal Sets.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
toJournalSets []     = pure J.emptyJSets
toJournalSets (x:xs) = do
    jKeys <- toJournalKeys x
    jSets <- traverse (toJournalSet jKeys) xs
    if duplicateKeys jSets
       then Left "There are duplicated journal set keys."
       else pure . J.pack $ jSets

---------------------------------------------------------------------
-- Journal keys

toJournalKeys :: [Text] -> Either String [Text]
-- ^The first row in the csv file is the journal abbreviations. The
-- first element is a dummy header for the journal set keys, so it
-- needs to be dropped. Each journal must have a reference issue
-- available for this to succeed.
toJournalKeys []     = Left "Missing CSV journal key headers."
toJournalKeys (_:[]) = Left "Missing CSV journal key headers."
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
toJournalSet js (x:xs) = T.JSet <$> parseKey x <*> toIssuesInSet js xs

parseKey :: Text -> Either String Int
-- ^Parse the journal set key, which is just an integer.
parseKey t = maybe err pure . readMaybeTxt . Tx.takeWhile (not . isSpace) $ t
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t

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
duplicateKeys = go . map T.jsKey
    where go []     = False
          go (x:xs) = elem x xs || go xs

invalidIssErr :: Text -> Int -> Int -> T.ErrString
invalidIssErr j v n = "Invalid issue: " <> jstr <> " " <> vstr <> ":" <> nstr
    where jstr = Tx.unpack j
          vstr = show v
          nstr = show n

toVolIssNo :: Text -> Either String (Int, Int)
-- ^Parse a 'volume:issue' text string to the numeric values.
toVolIssNo t = case traverse readMaybeTxt . Tx.splitOn ":" $ t of
                    Just (v:i:[]) -> pure (v,i)
                    _             -> Left "Cannot parse volume:issue"
