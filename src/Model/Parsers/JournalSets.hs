{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJsetsCsv
    , parseJsetsTxt
    , parseSelection
    , parseJsets
    ) where

import qualified Data.Text             as Tx
import qualified Data.Attoparsec.Text  as At
import qualified Model.Core.Types      as T
import qualified Model.Parsers.CSV     as CSV
import qualified Model.Journals        as J
import           Data.Bifunctor               ( bimap              )
import           Data.Char                    ( isSpace, isDigit   )
import           Data.Text                    ( Text               )
import           Model.Core.Core              ( readMaybeTxt       )
import           Control.Applicative          ( some, many,  (<|>) )

-- =============================================================== --
-- Main parsers

parseJsets :: [T.Issue] -> Text -> Either T.ErrString T.JournalSets
parseJsets refs x = parseJsetsCsv refs x <|> parseJsetsTxt refs x

parseJsetsCsv :: [T.Issue] -> Text -> Either T.ErrString T.JournalSets
-- ^Parse a properly formatted csv file to JournalSets.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
-- All issues must be valid and the first row must be the journals.
parseJsetsCsv refs x = let err = (<>) "Cannot parse CSV: "
                       in  bimap err id $ CSV.parseCSV x
                                          >>= toJournalSets
                                          >>= validate refs

parseJsetsTxt :: [T.Issue] -> Text -> Either T.ErrString T.JournalSets
-- ^Parse a properly formatted text file to JournalSets.
-- All issues must be valid on lookup.
parseJsetsTxt refs t = let err = (<>) "Cannot parse TXT: "
                       in  bimap err id $ At.parseOnly jsetsTxtParser t
                                          >>= validate refs

parseSelection :: [T.Issue] -> Text -> Either T.ErrString T.SelectionSet
parseSelection refs t = let err = (<>) "Cannot parse selection: "
                        in  bimap err id $ At.parseOnly selParser t
                                           >>= validateSel refs

-- =============================================================== --
-- Local types

type RawSelSet = ( Int, [(RawIssue, [T.PageNumber])] )

type RawIssue  = ( Text, Int, Int  )

type RawJSet   = ( Int, [RawIssue] )

-- =============================================================== --
-- Parse validation

validate :: [T.Issue] -> [RawJSet] -> Either T.ErrString T.JournalSets
validate refs js = mapM go js >>= packJSets
    where go (key, iss) = T.JSet key <$> mapM (validateIssue refs) iss

validateSel :: [T.Issue] -> RawSelSet -> Either T.ErrString T.SelectionSet
validateSel refs (key,xs) = T.SelSet <$> pure key <*> mapM go xs
    where go (r,ys) = (,) <$> validateIssue refs r <*> pure ys

validateIssue :: [T.Issue] -> RawIssue -> Either T.ErrString T.Issue
validateIssue refs (j,v,n) = maybe err pure . J.lookupIssue refs j $ (v,n)
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
    iss <- many (issueTxtParser <* At.skipSpace)
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
    pure (journal, volNo, issNo)

intParser :: At.Parser Int
intParser = some At.digit >>= pure . read

dateParser :: At.Parser (Int, Int, Int)
dateParser = (,,) <$> ( intParser <* At.char '-'  )
                  <*> ( intParser <* At.char '-'  )
                  <*> ( intParser                 )

-- =============================================================== --
-- Component parsers for issue selections

selParser :: At.Parser RawSelSet
selParser = do
    At.skipSpace
    key <- jsetKeyParser <* At.skipSpace
    xs  <- many issueSel
    At.skipSpace
    At.endOfInput
    pure (key, xs)

issueSel :: At.Parser (RawIssue, [T.PageNumber])
issueSel = do
    At.skipSpace
    iss <- issueTxtParser
    At.skipWhile At.isHorizontalSpace <* At.endOfLine
    indent <- At.takeWhile At.isHorizontalSpace
    if Tx.null indent
       then pure (iss, [])
       else do p  <- pageNumber
               At.skipWhile At.isHorizontalSpace *> At.endOfLine
               ps <- many (indentedPageNumber indent)
               pure $ (iss, p : ps)

indentedPageNumber :: Text -> At.Parser T.PageNumber
indentedPageNumber indent = do
    At.string indent
    pageNumber <* At.skipWhile At.isHorizontalSpace <* At.endOfLine

pageNumber :: At.Parser T.PageNumber
pageNumber = T.PageNumber <$> prefix <*> digits
    where prefix = Tx.unpack <$> At.takeTill isDigit
          digits = read <$> some At.digit

-- =============================================================== --
-- Component parsers for CSV

toJournalSets :: [[Text]] -> Either T.ErrString [RawJSet]
-- ^Convert a parsed CSV file to raw journal sets.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
toJournalSets []     = pure []
toJournalSets (x:xs) = do
    jKeys <- toJournalKeys x
    mapM (toJournalSet jKeys) xs

---------------------------------------------------------------------
-- Journal keys

toJournalKeys :: [Text] -> Either T.ErrString [Text]
-- ^The first row in the csv file is the journal abbreviations. The
-- first element is a dummy header for the journal set keys, so it
-- needs to be dropped.
toJournalKeys []     = Left "Missing CSV journal key headers."
toJournalKeys (_:[]) = Left "Missing CSV journal key headers."
toJournalKeys (_:ks) = pure ks

---------------------------------------------------------------------
-- Individual journal sets and issues

toJournalSet :: [Text] -> [Text] -> Either T.ErrString RawJSet
-- ^Convert all csv cell Text values to a raw journal set.
-- The journal set must begin with a correctly formatted key.
toJournalSet _ []      = Left "Missing key for journal set."
toJournalSet js (x:xs) = (,) <$> parseKey x <*> toIssues js xs

parseKey :: Text -> Either String Int
-- ^Parse the journal set key, which is just an integer.
parseKey t = maybe err pure . readMaybeTxt . Tx.takeWhile (not . isSpace) $ t
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t

toIssues :: [Text] -> [Text] -> Either T.ErrString [RawIssue]
-- ^Generate the issues for each journal in a csv line corresponding
-- to a single journal set. The first argument is the list of
-- journals. The second argument is the volume and issue numbers for
-- the corresponding journal in the same order.
toIssues js xs = fmap concat . sequence . zipWith go js $ ys
    where ys     = xs ++ replicate (length js - length xs) Tx.empty
          go j y = (mapM toVolIssNo . Tx.lines) y
                   >>= pure . map ( \ (v,n) -> (j,v,n) )

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

toVolIssNo :: Text -> Either T.ErrString (Int, Int)
-- ^Parse a 'volume:issue' text string to the numeric values.
toVolIssNo t = case traverse readMaybeTxt . Tx.splitOn ":" $ t of
                    Just (v:i:[]) -> pure (v,i)
                    _             -> Left "Cannot parse volume:issue"
