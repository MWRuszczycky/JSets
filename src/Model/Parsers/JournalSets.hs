{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseCollection
    , parseCollectionCsv
    , parseCollectionTxt
    , parseSelection
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

parseCollection :: [T.Issue] -> Text
                   -> Either T.ErrString (T.Collection T.Issue)
parseCollection refs x = parseCollectionCsv refs x
                         <|> parseCollectionTxt refs x

parseCollectionCsv :: [T.Issue] -> Text
                      -> Either T.ErrString (T.Collection T.Issue)
-- ^Parse a properly formatted csv file to a Collection.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
-- All issues must be valid and the first row must be the journals.
parseCollectionCsv refs x = let err = (<>) "Cannot parse CSV: "
                            in  bimap err id $ CSV.parseCSV x
                                               >>= toRawCollection
                                               >>= validate refs

parseCollectionTxt :: [T.Issue] -> Text
                      -> Either T.ErrString (T.Collection T.Issue)
-- ^Parse a properly formatted text file to a Collection.
-- All issues must be valid on lookup.
parseCollectionTxt refs t = let err = (<>) "Cannot parse TXT: "
                            in  bimap err id $ At.parseOnly collectionParser t
                                               >>= validate refs

parseSelection :: [T.Issue] -> Text
                  -> Either T.ErrString (T.JournalSet T.Selection)
parseSelection refs t = let err = (<>) "Cannot parse selection: "
                        in  bimap err id $ At.parseOnly selParser t
                                           >>= validateSel refs

-- =============================================================== --
-- Local types

-- | Set number and raw issues
type RawJSet   = ( Int, [RawIssue] )

-- | Set numebr, raw issues and selected page numbers
type RawSelSet = ( Int, [(RawIssue, [T.PageNumber])] )

-- | Journal abbreviation, volume number and issue number
type RawIssue  = ( Text, Int, Int  )

-- =============================================================== --
-- Parse validation

validate :: T.References -> [RawJSet]
            -> Either T.ErrString (T.Collection T.Issue)
validate refs js = mapM go js >>= packCollection
    where go (setNo, iss) = T.JSet setNo <$> mapM (validateIssue refs) iss

validateSel :: T.References -> RawSelSet
               -> Either T.ErrString (T.JournalSet T.Selection)
validateSel refs (setNo,xs) = T.JSet <$> pure setNo <*> mapM go xs
    where go (r,ys) = T.Selection <$> validateIssue refs r <*> pure ys

validateIssue :: T.References -> RawIssue -> Either T.ErrString T.Issue
validateIssue refs (j,v,n) = maybe err pure . J.lookupIssue refs j $ (v,n)
    where err = Left $ invalidIssErr j v n

packCollection :: [T.JournalSet T.Issue]
                  -> Either T.ErrString (T.Collection T.Issue)
packCollection js
    | duplicateSetNos js = Left "There are duplicated journal set keys."
    | otherwise          = pure . J.pack $ js

-- =============================================================== --
-- Component parsers for TXT

collectionParser :: At.Parser [RawJSet]
collectionParser = At.skipSpace *> many rawJSetParser <* At.endOfInput

rawJSetParser :: At.Parser RawJSet
rawJSetParser = do
    setNo  <- setNoParser
    issues <- many (rawIssueParser <* At.skipSpace)
    At.skipSpace
    pure (setNo, issues)

setNoParser :: At.Parser Int
setNoParser = do
    setNo <- intParser
    At.skipSpace *> At.char '|' *> At.skipSpace
    dateParser *> At.skipSpace
    pure setNo

rawIssueParser :: At.Parser RawIssue
rawIssueParser = do
    journal <- Tx.init <$> At.takeWhile1 ( not . isDigit )
    volNo   <- intParser <* At.skipSpace
    issNo   <- intParser <* At.skipSpace
    At.char '(' *> dateParser *> At.char ')'
    pure (journal, volNo, issNo)

-- =============================================================== --
-- Component parsers for issue selections

selParser :: At.Parser RawSelSet
selParser = do
    At.skipSpace
    key <- setNoParser <* At.skipSpace
    xs  <- many issueSel
    At.skipSpace
    At.endOfInput
    pure (key, xs)

issueSel :: At.Parser (RawIssue, [T.PageNumber])
issueSel = do
    At.skipSpace
    iss <- rawIssueParser
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

-- =============================================================== --
-- General component parsers

intParser :: At.Parser Int
intParser = some At.digit >>= pure . read

dateParser :: At.Parser (Int, Int, Int)
dateParser = (,,) <$> ( intParser <* At.char '-'  )
                  <*> ( intParser <* At.char '-'  )
                  <*> ( intParser                 )

pageNumber :: At.Parser T.PageNumber
pageNumber = do
    prefix <- Tx.unpack <$> pageNumberPrefix
    digits <- read <$> some At.digit
    pure $ T.PageNumber prefix digits

pageNumberPrefix :: At.Parser Text
pageNumberPrefix = At.takeTill (At.inClass " :\n\r\t") <* At.char ':'
                   <|> pure ""

-- =============================================================== --
-- Component parsers for CSV

toRawCollection :: [[Text]] -> Either T.ErrString [RawJSet]
-- ^Convert a parsed CSV file to a raw collection.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
toRawCollection []     = pure []
toRawCollection (x:xs) = do
    abbrs <- toJournalAbbrs x
    mapM (toRawJournalSet abbrs) xs

---------------------------------------------------------------------
-- Journal abbreviations

toJournalAbbrs :: [Text] -> Either T.ErrString [Text]
-- ^The first row in the csv file is the journal abbreviations. The
-- first element is a dummy header for the journal set keys, so it
-- needs to be dropped.
toJournalAbbrs []     = Left "Missing CSV journal key headers."
toJournalAbbrs (_:[]) = Left "Missing CSV journal key headers."
toJournalAbbrs (_:ks) = pure ks

---------------------------------------------------------------------
-- Individual journal sets and issues

toRawJournalSet :: [Text] -> [Text] -> Either T.ErrString RawJSet
-- ^Convert all csv cell Text values to a raw journal set.
-- The journal set must begin with a correctly formatted key.
toRawJournalSet _ []      = Left "Missing key for journal set."
toRawJournalSet abbrs (x:xs) = (,) <$> toSetNo x <*> toRawIssues abbrs xs

toSetNo :: Text -> Either String Int
-- ^Parse the journal set number, which is just an integer.
toSetNo t = maybe err pure . readMaybeTxt . Tx.takeWhile (not . isSpace) $ t
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t

toRawIssues :: [Text] -> [Text] -> Either T.ErrString [RawIssue]
-- ^Generate the issues for each journal in a csv line corresponding
-- to a single journal set. The first argument is the list of
-- journal abbreviations. The second argument is the volume and issue numbers
-- for the corresponding journal in the same order.
toRawIssues abbrs xs = fmap concat . sequence . zipWith go abbrs $ ys
    where ys     = xs ++ replicate (length abbrs - length xs) Tx.empty
          go j y = (mapM toVolIssNo . Tx.lines) y
                   >>= pure . map ( \ (v,n) -> (j,v,n) )

-- =============================================================== --
-- Helper functions

duplicateSetNos :: [T.JournalSet T.Issue] -> Bool
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
