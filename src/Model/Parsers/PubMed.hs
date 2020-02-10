{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.PubMed
    ( parseToC
    ) where

import qualified Model.Core.Types     as T
import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text       )
import           Data.Char                   ( isSpace    )
import           Data.Bifunctor              ( bimap      )
import           Control.Applicative         ( many, some )

parseToC :: T.Issue -> Text -> Either String T.TableOfContents
parseToC iss = bimap err id . At.parseOnly (tableOfContents iss)
    where err x = "Cannot parse toc: " ++ x

tableOfContents :: T.Issue -> At.Parser T.TableOfContents
tableOfContents iss = do
    At.skipSpace *> skipXML
    At.skipSpace *> skipXML
    At.skipSpace *> At.string "<pre>"
    At.skipSpace
    xs <- many $ citation iss
    At.skipSpace *> At.string "</pre>"
    At.skipSpace *> At.endOfInput
    pure xs

skipXML :: At.Parser ()
skipXML = At.char '<' *> At.skipWhile (/= '>') *> At.char '>' *> pure ()

dotSep :: At.Parser ()
dotSep = At.char '.' *> At.skipSpace

authorList :: At.Parser Text
authorList = At.takeTill (== '.') <* dotSep

title :: T.Issue -> At.Parser Text
title iss = do
    x <- At.takeTill (== '.') <* dotSep
    if x == (T.pubmed . T.journal) iss
       then pure Tx.empty
       else do rest <- title iss
               if Tx.null rest
                  then pure x
                  else pure $ x <> ". " <> rest

issueInformation :: At.Parser ()
issueInformation = do
    some At.digit  *> At.skipSpace              -- year
    some At.letter *> At.skipSpace              -- month
    some At.digit  <* At.char ';'               -- day
    some At.digit                               -- volume number
    At.char '(' *> some At.digit <* At.char ')' -- issue number
    At.char ':'
    pure ()

pageNumbers :: At.Parser (Int,Int)
pageNumbers = do
    p1 <- read <$> some At.digit
    At.char '-'
    p2 <- read <$> some At.digit
    dotSep
    pure (p1, p2)

doiUrl :: At.Parser Tx.Text
doiUrl = do
    At.string "doi:"
    At.skipSpace
    doi <- Tx.init <$> At.takeTill isSpace
    pure $ "https://www.doi.org/" <> doi

info :: At.Parser ()
info = do
    At.takeTill At.isEndOfLine
    At.endOfLine
    x <- At.peekChar'
    if At.isEndOfLine x
       then At.endOfLine
       else info

stripNewLines :: Tx.Text -> Tx.Text
stripNewLines = Tx.unwords . Tx.lines

citation :: T.Issue -> At.Parser T.Citation
citation iss = do
    At.skipSpace
    some At.digit *> At.char ':' *> At.skipSpace
    authors    <- stripNewLines <$> authorList
    theTitle   <- stripNewLines <$> title iss
    issueInformation
    pages      <- pageNumbers
    doi        <- doiUrl
    info
    pure $ T.Citation { T.title   = theTitle
                      , T.authors = authors
                      , T.issue   = iss
                      , T.pages   = pages
                      , T.doi     = doi
                      }
