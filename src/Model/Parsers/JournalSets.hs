{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseCollection
    -- , parseCollectionCsv
    -- , parseCollectionTxt
    -- , parseSelection
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
                   -> Either T.ErrString (T.Collection T.Selection)
parseCollection refs x = parseCollectionCsv refs x
                         <|> parseSelections refs x

parseCollectionCsv :: [T.Issue] -> Text
                      -> Either T.ErrString (T.Collection T.Selection)
-- ^Parse a properly formatted csv file to a Collection.
-- The csv file should not contain any empty rows between sets. Empty
-- csv cells are treated as no issues for the corresponding journal.
-- All issues must be valid and the first row must be the journals.
parseCollectionCsv refs x = let err = (<>) "Cannot parse CSV: "
                            in  bimap err id $ CSV.parseCSV x
                                               >>= toRawCollection
                                               >>= validate refs

parseSelections :: [T.Issue] -> Text
                   -> Either T.ErrString (T.Collection T.Selection)
parseSelections refs t = let err = (<>) "Cannot parse selection: "
                         in  bimap err id $ At.parseOnly rawJsets t
                                            >>= validate refs

-- =============================================================== --
-- Local types

-- | Set number and raw selections
type RawJset      = ( Int, [RawSelection] )

-- | Raw issues and selected page numbers from that issue
type RawSelection = (RawIssue, [T.PageNumber])

-- | Journal abbreviation, volume number and issue number
type RawIssue     = ( Text, Int, Int )

-- =============================================================== --
-- Parse validation

validate :: T.References -> [RawJset]
            -> Either T.ErrString (T.Collection T.Selection)
validate refs xs = mapM (validateSelection refs) xs >>= packCollection
    -- where go (setNo, iss) = T.JSet setNo <$> mapM (validateSelection refs) iss

validateSelection :: T.References -> RawJset
                     -> Either T.ErrString (T.JournalSet T.Selection)
validateSelection refs (setNo,xs) = T.JSet <$> pure setNo <*> mapM go xs
    where go (r,ys) = T.Selection <$> validateIssue refs r <*> pure ys

validateIssue :: T.References -> RawIssue -> Either T.ErrString T.Issue
validateIssue refs (j,v,n) = maybe err pure . J.lookupIssue refs j $ (v,n)
    where err = Left $ invalidIssErr j v n

packCollection :: [T.JournalSet T.Selection]
                  -> Either T.ErrString (T.Collection T.Selection)
packCollection js
    | duplicateSetNos js = Left "There are duplicated journal set keys."
    | otherwise          = pure . J.pack $ js

-- =============================================================== --
-- Component parsers for TXT

setNoParser :: At.Parser Int
setNoParser = do
    setNo <- intParser
    At.skipSpace *> At.char '|' *> At.skipSpace
    dateParser *> At.skipSpace
    pure setNo

rawIssue :: At.Parser RawIssue
rawIssue = do
    journal <- Tx.init <$> At.takeWhile1 ( not . isDigit )
    volNo   <- intParser <* At.skipSpace
    issNo   <- intParser <* At.skipSpace
    At.char '(' *> dateParser *> At.char ')'
    pure (journal, volNo, issNo)

-- =============================================================== --
-- Component parsers for issue selections

rawJsets :: At.Parser [RawJset]
rawJsets = many rawJset <* At.endOfInput

rawJset :: At.Parser RawJset
rawJset = do
    At.skipSpace
    setNo <- setNoParser <* At.skipSpace
    xs    <- many rawSelection
    At.skipSpace
    pure (setNo, xs)

rawSelection :: At.Parser RawSelection
rawSelection = do
    iss <- rawIssue
    At.skipWhile At.isHorizontalSpace <* At.endOfLine
    indent <- At.takeWhile At.isHorizontalSpace
    if Tx.null indent
       then pure (iss, [])
       else do p  <- pageNumber
               At.skipWhile At.isHorizontalSpace *> At.endOfLine
               ps <- many (indentedPageNumber indent)
               At.skipSpace
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

toRawCollection :: [[Text]] -> Either T.ErrString [RawJset]
-- ^Convert a parsed CSV file to a raw collection.
-- The input is a list of lists of Text, where each sublist is a row
-- in the CSV file and each Text is a cell in that row.
toRawCollection []     = pure []
toRawCollection (x:xs) = do
    abbrs <- toJournalAbbrs x
    mapM (toRawJset abbrs) xs

toRawJset :: [Text] -> [Text] -> Either T.ErrString RawJset
-- ^Convert all csv cell Text values to a raw selection.
-- The journal set must begin with a correctly formatted key.
toRawJset _     []     = Left "Missing key for journal set."
toRawJset abbrs (x:xs) = (,) <$> toSetNo x <*> toRawSelection abbrs xs

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

toSetNo :: Text -> Either String Int
-- ^Parse the journal set number, which is just an integer.
toSetNo t = maybe err pure . readMaybeTxt . Tx.takeWhile (not . isSpace) $ t
    where err = Left $ "Invalid journal set key: " ++ Tx.unpack t

toRawSelection :: [Text] -> [Text] -> Either T.ErrString [RawSelection]
-- ^Generate the issues for each journal in a csv line corresponding
-- to a single journal set. The first argument is the list of
-- journal abbreviations. The second argument is the volume and issue numbers
-- for the corresponding journal in the same order.
toRawSelection abbrs xs = fmap concat . sequence . zipWith go abbrs $ ys
    where ys     = xs ++ replicate (length abbrs - length xs) Tx.empty
          go j y = (mapM toVolIssNo . Tx.lines) y
                   >>= pure . map ( \ (v,n) -> ( (j,v,n), [] ) )

-- =============================================================== --
-- Helper functions

duplicateSetNos :: [T.JournalSet a] -> Bool
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
