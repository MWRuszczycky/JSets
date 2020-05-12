{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.PubMed
    ( parseCitations
    , parseCited
    , noCitations
    ) where

import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import qualified Data.Char            as Ch
import           Data.Text                   ( Text       )
import           Data.List                   ( sortBy     )
import           Data.Bifunctor              ( bimap      )
import           Control.Applicative         ( many, some )
import           Data.Ord                    ( comparing  )


parseCitations :: T.HasIssue a => a -> Text -> Either String [T.Citation]
parseCitations iss = bimap err sortByPage . At.parseOnly (citations iss)
    where err x = "Cannot parse PubMed table of contents: " ++ x

parseCited :: T.Selection -> Text -> Either String T.IssueContent
parseCited iss ts = parseCitations iss ts >>= pure . T.IssueContent iss

noCitations :: Text -> Either String Bool
noCitations = bimap err id . At.parseOnly isEmpty
    where err x = "Cannot parse PubMed table of contents: " ++ x

citations :: T.HasIssue a => a -> At.Parser [T.Citation]
citations iss = do
    At.skipSpace *> skipXML
    At.skipSpace *> skipXML
    At.skipSpace *> At.string "<pre>"
    At.skipSpace
    xs <- many $ citation iss
    At.skipSpace *> At.string "</pre>"
    At.skipSpace *> At.endOfInput
    pure xs

isEmpty :: At.Parser Bool
isEmpty = do
    At.skipSpace *> skipXML
    At.skipSpace *> skipXML
    At.skipSpace *> At.string "<pre>"
    x <- At.choice [ At.string "</pre>", pure Tx.empty ]
    pure $ x == "</pre>"

---------------------------------------------------------------------
-- Helper functions

skipXML :: At.Parser ()
skipXML = At.char '<' *> At.skipWhile (/= '>') *> At.char '>' *> pure ()

dotSep :: At.Parser ()
dotSep = At.char '.' *> At.skipSpace

stripNewLines :: Tx.Text -> Tx.Text
stripNewLines = Tx.unwords . Tx.words

matchesTitle :: T.HasIssue a => a -> Text -> Bool
matchesTitle iss = (==) $ (T.pubmed . T.journal) iss

sortByPage :: [T.Citation] -> [T.Citation]
sortByPage = sortBy (comparing pageNumbers)
    where pageNumbers = fst . T.pages

correctPages :: Maybe (T.PageNumber, T.PageNumber) -> Text
                -> (T.PageNumber, T.PageNumber)
correctPages (Just ps) _ = ps
correctPages Nothing doi = ( T.PageNumber t 1, T.PageNumber t 0 )
    where t = Tx.unpack
              . Tx.reverse
              . Tx.takeWhile (/= '.')
              . Tx.reverse $ doi

---------------------------------------------------------------------
-- Parsers

citation :: T.HasIssue a => a -> At.Parser T.Citation
citation iss = do
    At.skipSpace
    some At.digit *> At.char ':' *> At.skipSpace
    (authors, title) <- authorsAndTitle iss
    issueData
    pages            <- pageNumbers
    doi              <- doiUrl
    pubmedData
    pure $ T.Citation { T.title   = title
                      , T.authors = authors
                      , T.pages   = correctPages pages doi
                      , T.doi     = doi
                      }

authorsAndTitle :: T.HasIssue a => a -> At.Parser (Text, Text)
authorsAndTitle iss = go <$> frontMatter iss
    where go []     = ( "No authors listed", "No title" )
          go (x:[]) = ( "No authors listed", stripNewLines x )
          go (x:xs) = ( x, Tx.unwords $ xs )

frontMatter :: T.HasIssue a => a -> At.Parser [Text]
frontMatter iss = do
    x <- stripNewLines <$> At.takeTill (At.inClass ".?!")
    p <- At.satisfy  (At.inClass ".?!") <* At.skipSpace
    if matchesTitle iss x
       then pure []
       else (Tx.snoc x p :) <$> frontMatter iss

issueData :: At.Parser ()
issueData = do
    some At.digit  *> At.skipSpace              -- year
    some At.letter *> At.skipSpace              -- month
    many At.digit  <* At.char ';'               -- day
    some At.digit                               -- volume number
    At.char '(' *> some At.digit <* At.char ')' -- issue number
    pure ()

pageNumber :: At.Parser T.PageNumber
pageNumber = T.PageNumber <$> prefix <*> digits
    where prefix = Tx.unpack <$> At.takeTill Ch.isDigit
          digits = read <$> some At.digit

pageNumbers :: At.Parser (Maybe (T.PageNumber, T.PageNumber) )
pageNumbers = do
    p1 <- At.option Nothing $ Just <$> (At.char ':' *> pageNumber)
    x  <- At.choice [ At.char '-', At.char '.' ]
    if x == '.'
       then At.skipSpace *> pure ( (,) <$> p1 <*> p1 )
       else do p2 <- Just <$> pageNumber
               dotSep
               pure $ (,) <$> p1 <*> p2

doiUrl :: At.Parser Tx.Text
doiUrl = do
    At.manyTill At.anyChar $ At.string "doi:"
    At.skipSpace
    doi <- Tx.init <$> At.takeTill Ch.isSpace
    pure $ "https://www.doi.org/" <> doi

pubmedData :: At.Parser ()
pubmedData = do
    At.takeTill At.isEndOfLine
    At.endOfLine
    x <- At.peekChar'
    if At.isEndOfLine x
       then At.endOfLine
       else pubmedData
