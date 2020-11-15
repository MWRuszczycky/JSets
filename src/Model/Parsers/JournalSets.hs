{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJSets
    ) where

import qualified Data.Text             as Tx
import qualified Data.Attoparsec.Text  as At
import qualified Model.Core.Types      as T
import qualified Model.Parsers.CSV     as CSV
import qualified Model.Parsers.Core    as P
import qualified Model.Journals        as J
import           Data.Bifunctor               ( bimap              )
import           Data.Text                    ( Text               )
import           Model.Core.Core              ( readMaybeTxt       )
import           Control.Applicative          ( some, many,  (<|>) )
import           Data.Char                    ( isSpace, isDigit
                                              , isAlphaNum         )

-- =============================================================== --
-- Main parsers

parseJSets :: [T.Issue] -> Text -> Either T.ErrString (T.JSets T.Issue)
-- ^Parse JSets from a csv or text file. First attempts to parse from
-- csv and if that fails then tries to parse from text.
parseJSets refs x = parseCsv refs x <|> parseTxt refs x

parseCsv :: [T.Issue] -> Text -> Either T.ErrString (T.JSets T.Issue)
-- ^Parse a properly formatted csv file to a collection of JSets.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
-- All issues must be valid and the first row must be the journals.
parseCsv refs x = let err = (<>) "Cannot parse CSV: "
                  in  bimap err id $ CSV.parse x
                                     >>= csvCollection
                                     >>= validate refs

parseTxt :: [T.Issue] -> Text -> Either T.ErrString (T.JSets T.Issue)
-- ^Parse a properly formatted text file to a Collection.
parseTxt refs t = let err = (<>) "Cannot parse selection: "
                  in  bimap err id $ At.parseOnly txtJSets t >>= validate refs

-- =============================================================== --
-- Parsed journal set/issue validation and construction

-- CSV and text formatted journal sets and selections are first
-- parsed to just get the information into a structured data type
-- denoted with a prime. These structured values are then validated
-- and the actual issue and journal set values are constructed.

---------------------------------------------------------------------
-- Local helper types

-- | Unvalidated JSet
type JSet' = ( Int, [Issue'], [T.PMID] )

-- | Unvalidated Issue: journal abbreviation, volume and issue.
type Issue' = ( Text, Int, Int )

---------------------------------------------------------------------
-- Validation

validate :: T.References -> [JSet'] -> Either T.ErrString (T.JSets T.Issue)
validate refs js = mapM (validateJSet refs) js >>= packJSets

validateJSet :: T.References -> JSet' -> Either T.ErrString (T.JSet T.Issue)
validateJSet refs (n,xs,ids) = T.JSet <$> pure n
                                      <*> mapM (validateIssue refs) xs
                                      <*> pure ids

validateIssue :: T.References -> Issue' -> Either T.ErrString T.Issue
validateIssue refs (j,v,n) = maybe err pure . J.lookupIssue refs j $ (v,n)
    where err = Left $ invalidIssErr j v n

packJSets :: [T.JSet T.Issue]-> Either T.ErrString (T.JSets T.Issue)
packJSets js
    | duplicateSetNos js = Left "There are duplicated journal set numbers."
    | otherwise          = pure . J.pack $ js

-- =============================================================== --
-- Component parsers for TXT

txtJSets :: At.Parser [JSet']
-- ^Collection (i.e., list) of unvalidated JSets.
txtJSets = many txtJSet <* At.skipSpace <* At.endOfInput

txtJSet :: At.Parser JSet'
-- ^Single unvalidated JSet.
txtJSet = do
    At.skipSpace
    n <- setNumber
    At.skipSpace
    (xs,ids) <- unzip <$> many selection
    pure (n, xs, concat ids)

setNumber :: At.Parser Int
setNumber = do
    setNo <- P.unsigned'
    P.pipe'
    ( P.dateN *> pure () ) <|> P.spacesToEoL
    pure setNo

selection :: At.Parser (Issue', [T.PMID])
-- ^An unvalidated Issue and associated PMIDs.
selection = do
    iss <- txtIssue
    P.spacesToEoL
    ids <- indentedPMIDs
    At.skipSpace
    pure (iss, ids)

txtIssue :: At.Parser Issue'
txtIssue = do
    journal <- Tx.init <$> At.takeWhile1 ( not . isDigit )
    volNo <- P.unsigned'
    P.colon'
    issNo <- P.unsigned'
    P.horizontalSpaces
    ( P.dateP *> pure () ) <|> pure ()
    pure (journal, volNo, issNo)

indentedPMIDs :: At.Parser [T.PMID]
indentedPMIDs = At.option [] go
    where go = do indent <- Tx.pack <$> some ( At.char ' ' )
                  x      <- pmidToEoL
                  xs     <- many $ At.string indent *> pmidToEoL
                  pure $ x : xs

pmidToEoL :: At.Parser T.PMID
pmidToEoL = Tx.pack <$> some (At.satisfy isAlphaNum) <* P.spacesToEoL

-- =============================================================== --
-- Component parsers for CSV

csvCollection :: CSV.CSV -> Either T.ErrString [JSet']
-- ^Convert a parsed CSV file to an unvalidated collection of JSets.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
csvCollection []     = pure []
csvCollection (x:xs) = do
    abbrs <- csvJournalAbbrs x
    mapM (csvJSet abbrs) xs

csvJournalAbbrs :: [Text] -> Either T.ErrString [Text]
-- ^The first row in the csv file is the journal abbreviations. The
-- first element is a dummy header for the journal set keys, so it
-- needs to be dropped.
csvJournalAbbrs []     = Left "Missing CSV journal key headers."
csvJournalAbbrs (_:[]) = Left "Missing CSV journal key headers."
csvJournalAbbrs (_:ks) = pure ks

csvJSet :: [Text] -> [Text] -> Either T.ErrString JSet'
-- ^Convert all csv cell Text values to a raw selection.
-- The journal set must begin with a correctly formatted key.
csvJSet _     []     = Left "Missing key for journal set."
csvJSet abbrs (x:xs) = (,,) <$> csvSetNo x <*> csvIssue abbrs xs <*> pure []

---------------------------------------------------------------------
-- Individual journal sets and issues

csvSetNo :: Text -> Either String Int
-- ^Parse the journal set number, which is just an integer.
csvSetNo t = maybe err pure . readMaybeTxt . Tx.takeWhile (not . isSpace) $ t
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t

csvIssue :: [Text] -> [Text] -> Either T.ErrString [Issue']
-- ^Generate the issues for each journal in a csv line corresponding
-- to a single journal set. The first argument is the list of
-- journal abbreviations. The second argument is the volume and issue numbers
-- for the corresponding journal in the same order.
csvIssue abbrs xs = fmap concat . sequence . zipWith go abbrs $ ys
    where ys     = xs ++ replicate (length abbrs - length xs) Tx.empty
          go j y = (mapM toVolIssNo . Tx.lines) y
                   >>= pure . map ( \ (v,n) -> (j,v,n) )

-- =============================================================== --
-- Helper functions

duplicateSetNos :: [T.JSet T.Issue] -> Bool
-- ^Check for duplicated journal set numbers.
duplicateSetNos = go . map T.setNo
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
