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

parseJSets :: [T.Issue] -> Text -> Either T.ErrString (T.JSets T.Selection)
parseJSets refs x = parseCsv refs x <|> parseTxt refs x

parseCsv :: [T.Issue] -> Text -> Either T.ErrString (T.JSets T.Selection)
-- ^Parse a properly formatted csv file to a Collection.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
-- All issues must be valid and the first row must be the journals.
parseCsv refs x = let err = (<>) "Cannot parse CSV: "
                  in  bimap err id $ CSV.parse x
                                     >>= getRawCollection
                                     >>= validate refs

parseTxt :: [T.Issue] -> Text -> Either T.ErrString (T.JSets T.Selection)
-- ^Parse a properly formatted text file to a Collection.
parseTxt refs t = let err = (<>) "Cannot parse selection: "
                  in  bimap err id $ At.parseOnly rawJsets t >>= validate refs

-- =============================================================== --
-- Local types
-- CSV and text formatted journal sets and selections are first
-- parsed to just get the information into a 'raw' structured data
-- type. These structured, raw values are then validated and the
-- actual issue and journal set values are constructed.

-- | Set number and raw selections
type RawJset      = ( Int, [RawSelection] )

-- | Raw issues and selected page numbers from that issue
type RawSelection = (RawIssue, [T.PMID])

-- | Journal abbreviation, volume number and issue number
type RawIssue     = ( Text, Int, Int )

-- =============================================================== --
-- Parsed journal set/issue validation and construction

validate :: T.References -> [RawJset] -> Either T.ErrString (T.JSets T.Selection)
validate refs xs = mapM (validateJset refs) xs >>= packJSets

validateJset :: T.References -> RawJset -> Either T.ErrString (T.JSet T.Selection)
validateJset refs (setNo,xs) = T.JSet <$> pure setNo <*> mapM go xs
    where go (r,ys) = T.Selection <$> validateIssue refs r <*> pure ys

validateIssue :: T.References -> RawIssue -> Either T.ErrString T.Issue
validateIssue refs (j,v,n) = maybe err pure . J.lookupIssue refs j $ (v,n)
    where err = Left $ invalidIssErr j v n

packJSets :: [T.JSet T.Selection] -> Either T.ErrString (T.JSets T.Selection)
packJSets js
    | duplicateSetNos js = Left "There are duplicated journal set keys."
    | otherwise          = pure . J.pack $ js

-- =============================================================== --
-- Component parsers for TXT

rawJsets :: At.Parser [RawJset]
rawJsets = many rawJset <* At.skipSpace <* At.endOfInput

rawJset :: At.Parser RawJset
rawJset = do
    At.skipSpace
    n  <- setNoParser
    At.skipSpace
    xs <- many rawSelection
    pure (n, xs)

setNoParser :: At.Parser Int
setNoParser = do
    setNo <- P.unsigned'
    P.pipe'
    ( P.dateN *> pure () ) <|> P.spacesToEoL
    pure setNo

rawSelection :: At.Parser RawSelection
rawSelection = do
    iss <- rawIssue
    P.spacesToEoL
    ids <- indentedPMIDs
    At.skipSpace
    pure (iss, ids)

rawIssue :: At.Parser RawIssue
rawIssue = do
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

getRawCollection :: CSV.CSV -> Either T.ErrString [RawJset]
-- ^Convert a parsed CSV file to a raw collection.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
getRawCollection []     = pure []
getRawCollection (x:xs) = do
    abbrs <- getJournalAbbrs x
    mapM (getRawJset abbrs) xs

getJournalAbbrs :: [Text] -> Either T.ErrString [Text]
-- ^The first row in the csv file is the journal abbreviations. The
-- first element is a dummy header for the journal set keys, so it
-- needs to be dropped.
getJournalAbbrs []     = Left "Missing CSV journal key headers."
getJournalAbbrs (_:[]) = Left "Missing CSV journal key headers."
getJournalAbbrs (_:ks) = pure ks

getRawJset :: [Text] -> [Text] -> Either T.ErrString RawJset
-- ^Convert all csv cell Text values to a raw selection.
-- The journal set must begin with a correctly formatted key.
getRawJset _     []     = Left "Missing key for journal set."
getRawJset abbrs (x:xs) = (,) <$> getSetNo x <*> getRawSelection abbrs xs

---------------------------------------------------------------------
-- Individual journal sets and issues

getSetNo :: Text -> Either String Int
-- ^Parse the journal set number, which is just an integer.
getSetNo t = maybe err pure . readMaybeTxt . Tx.takeWhile (not . isSpace) $ t
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t

getRawSelection :: [Text] -> [Text] -> Either T.ErrString [RawSelection]
-- ^Generate the issues for each journal in a csv line corresponding
-- to a single journal set. The first argument is the list of
-- journal abbreviations. The second argument is the volume and issue numbers
-- for the corresponding journal in the same order.
getRawSelection abbrs xs = fmap concat . sequence . zipWith go abbrs $ ys
    where ys     = xs ++ replicate (length abbrs - length xs) Tx.empty
          go j y = (mapM toVolIssNo . Tx.lines) y
                   >>= pure . map ( \ (v,n) -> ( (j,v,n), [] ) )

-- =============================================================== --
-- Helper functions

duplicateSetNos :: [T.JSet a] -> Bool
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
