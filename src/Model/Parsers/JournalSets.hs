{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJSets
    ) where

import qualified Data.Attoparsec.Text  as At
import qualified Data.Text             as Tx
import qualified Model.Core.Types      as T
import qualified Model.Journals        as J
import qualified Model.Parsers.CSV     as CSV
import qualified Model.Parsers.Core    as P
import           Control.Monad                ( when                 )
import           Data.Bifunctor               ( bimap                )
import           Data.Char                    ( isSpace, isDigit
                                              , isAlphaNum           )
import           Data.Maybe                   ( catMaybes, isNothing )
import           Data.Text                    ( Text                 )
import           Control.Applicative          ( some, many,  (<|>)   )
import           Model.Core.Core              ( readMaybeTxt         )

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
type JSet' = ( Int, [(Issue', [Selection'])] )

-- | Unvalidated Issue: journal abbreviation, volume, issue
type Issue' = ( Text, Int, Int )

-- | Unvalidated Selection: selection key, selection value
-- The key is either empty (PMID), 'add' (value associated with issue)
-- 'doi' (value is a DOI) or 'link' (value is a direct link)
type Selection' = (Text, Text)

---------------------------------------------------------------------
-- Validation

validate :: T.References -> [JSet'] -> Either T.ErrString (T.JSets T.Issue)
validate refs js = mapM (validateJSet refs) js >>= packJSets

validateJSet :: T.References -> JSet' -> Either T.ErrString (T.JSet T.Issue)
validateJSet refs (n,xs) = do
    (mbIss, sel) <- unzip <$> mapM (validateIssue refs) xs
    pure $ T.JSet n (catMaybes mbIss) (concat sel)

validateIssue :: T.References -> (Issue', [Selection'])
                 -> Either T.ErrString (Maybe T.Issue, [T.Selection])
validateIssue _ ( ("Extra Articles",_,_) , xs ) =
    (,) Nothing <$> mapM (readSelection Nothing) xs
validateIssue refs ( (j,v,n), xs ) = do
    let iss = J.lookupIssue refs j (v,n)
    when (isNothing iss) . Left $ invalidIssErr j v n
    sel <- mapM (readSelection iss) xs
    pure (iss, sel)

readSelection :: Maybe T.Issue -> Selection' -> Either T.ErrString T.Selection
readSelection (Just iss) ("add", x) = pure $ T.FromIssue iss x
readSelection (Just iss) ("",    x) = pure $ T.ByBndPMID iss x
readSelection (Just iss) ("doi", x) = pure $ T.ByBndDOI  iss x
readSelection _          ("web", x) = pure $ T.ByLink        x
readSelection _          ("",    x) = pure $ T.ByPMID        x
readSelection _          ("doi", x) = pure $ T.ByDOI         x
readSelection _          (y,     x) = Left err
    where err = unwords [ "Unable to read selection.\nInvalid selector key '"
                        , Tx.unpack y, "' with value '" <> Tx.unpack x <> "'."
                        ]

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
    n  <- setNumber
    At.skipSpace
    xs <- many issueSelection
    pure (n, xs)

setNumber :: At.Parser Int
setNumber = do
    setNo <- P.unsigned'
    P.pipe'
    ( P.dateN *> pure () ) <|> P.spacesToEoL
    pure setNo

issueSelection :: At.Parser (Issue', [Selection'])
-- ^An unvalidated Issue and associated unvalidated selections.
issueSelection = do
    iss <- specificIssue <|> extraCitations
    P.spacesToEoL
    xs  <- indentedSelections
    At.skipSpace
    pure (iss, xs)

specificIssue :: At.Parser Issue'
specificIssue = do
    journal <- Tx.init <$> At.takeWhile1 ( not . isDigit )
    volNo <- P.unsigned'
    P.colon'
    issNo <- P.unsigned'
    P.horizontalSpaces
    ( P.dateP *> pure () ) <|> pure ()
    pure (journal, volNo, issNo)

extraCitations :: At.Parser Issue'
extraCitations = (,,) <$> At.string "Extra Articles" <*> pure 0 <*> pure 0

indentedSelections :: At.Parser [(Text, Text)]
indentedSelections = At.option [] go
    where go = do indent <- Tx.pack <$> some ( At.char ' ' )
                  x      <- selectionToEoL
                  xs     <- many $ At.string indent *> selectionToEoL
                  pure $ x : xs

selectionToEoL :: At.Parser (Text, Text)
selectionToEoL = keyed <|> unKeyed
    where unKeyed = do pmid <- some (At.satisfy isAlphaNum) <* P.spacesToEoL
                       pure (Tx.empty, Tx.pack pmid)
          keyed   = do key <- some (At.satisfy isAlphaNum) <* P.colon'
                       val <- At.takeTill At.isEndOfLine   <* At.endOfLine
                       pure (Tx.pack key, val)

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
csvJSet abbrs (x:xs) = do
    n   <- csvSetNo x
    iss <- csvIssue abbrs xs
    pure ( n, zip iss . repeat $ [] )

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
