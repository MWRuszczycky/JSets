{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.PubMed
    ( parseCitations
    , parsePMIDs
    ) where

import qualified Data.Attoparsec.Text as At
import qualified Data.Text            as Tx
import qualified Data.Map.Strict      as Map
import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Model.Parsers.JSON   as JS
import qualified Model.Parsers.Core   as P
import           Control.Applicative          ( (<|>), many        )
import           Data.Text                    ( Text               )
import           Data.Time                    ( Day, fromGregorian )

-- =============================================================== --
-- Parsing PubMed Entrez ESearch results

parsePMIDs :: Text -> Either T.ErrString [T.PMID]
parsePMIDs json = JS.parse json >>= maybe err pure . go
    where err = Left "Unable to read idlist from PMID request!"
          go  = JS.lookupListWith [ "esearchresult", "idlist" ] JS.str

-- =============================================================== --
-- Parsing PubMed Entrez ESummary results

parseCitations :: [T.PMID] -> Text -> Either T.ErrString ([T.PMID], T.Citations)
-- ^Parse eSummary json from PubMed to construct citations. If any
-- pmids are indexed in the json but do not have corresponding
-- document summaries, these pmids are also returned. These represent
-- requested PMIDs that were not found (e.g., if incorrect).
parseCitations pmids txt = do
    json  <- JS.parse txt
    let (cs, missing) = C.splitMaybe . map (getCitation json) $ pmids
    pure (missing, Map.fromList cs)

---------------------------------------------------------------------
-- Components

getCitation :: JS.JSON -> T.PMID -> (T.PMID, Maybe T.Citation)
getCitation json pmid = (pmid, c)
    where c = do doc <- JS.lookupWith [ "result", pmid ] pure json
                 T.Citation <$> JS.lookupWith [ "title" ] JS.str doc
                            <*> getAuthors doc
                            <*> getIssue   doc
                            <*> getPages   doc
                            <*> getDoi     doc
                            <*> pure       pmid

getAuthors :: JS.JSON -> Maybe [Text]
getAuthors json = JS.lookupListWith [ "authors" ] go json
    where go = JS.lookupWith [ "name" ] JS.str

getIssue :: JS.JSON -> Maybe T.Issue
getIssue json = T.Issue <$> getDate json
                        <*> getInt [ "volume" ] json
                        <*> getInt [ "issue"  ] json
                        <*> getJournal json

getDate :: JS.JSON -> Maybe Day
getDate json = dateTxt >>= either (const Nothing) pure . At.parseOnly parseDate
    where dateTxt = JS.lookupWith [ "pubdate" ] JS.str json
                    <|> JS.lookupWith ["epubdate" ] JS.str json

getInt :: [Text] -> JS.JSON -> Maybe Int
getInt keys json = mbInt <|> Just 0
    where mbInt = JS.lookupWith keys JS.str json >>= C.readMaybeTxt

-- This needs to be fixed. It defaults to a Weekly/Resets publication
-- frequency.
getJournal :: JS.JSON -> Maybe T.Journal
getJournal json = JS.lookupWith [ "source" ] JS.str json >>= pure . go
    where go j = T.Journal j j j T.Weekly True

getDoi :: JS.JSON -> Maybe Text
getDoi json = JS.lookupWith [ "elocationid" ] JS.str json >>= go
    where prefix = "https://www.doi.org/"
          go x   = case Tx.take 4 x of
                        ""     -> pure Tx.empty
                        "doi:" -> pure . (prefix <>) . Tx.drop 5 $ x
                        _      -> go . Tx.tail $ x

getPages :: JS.JSON -> Maybe T.PageRange
getPages json = do
    txt <- JS.lookupWith [ "pages" ] JS.str json
    case At.parseOnly parsePageNumbers txt of
         Right ps -> pure ps
         Left  _  -> pure T.Online

parsePageNumbers :: At.Parser T.PageRange
parsePageNumbers = do
    prefix0 <- many At.letter
    suffix0 <- P.unsigned'
    At.char '-'
    prefix1 <- many At.letter
    suffix1 <- P.unsigned'
    pure $ T.InPrint (T.PageNo prefix0 suffix0) (T.PageNo prefix1 suffix1)

parseDate :: At.Parser Day
parseDate = fromGregorian <$> P.unsigned <*> month <*> day
    where month  = At.skipSpace *> At.choice aMonth <* At.skipSpace
          day    = P.unsigned' <|> pure 0
          aMonth = [ At.string "Jan" *> At.skipSpace *> pure 1
                   , At.string "Feb" *> At.skipSpace *> pure 2
                   , At.string "Mar" *> At.skipSpace *> pure 3
                   , At.string "Apr" *> At.skipSpace *> pure 4
                   , At.string "May" *> At.skipSpace *> pure 5
                   , At.string "Jun" *> At.skipSpace *> pure 6
                   , At.string "Jul" *> At.skipSpace *> pure 7
                   , At.string "Aug" *> At.skipSpace *> pure 8
                   , At.string "Sep" *> At.skipSpace *> pure 9
                   , At.string "Oct" *> At.skipSpace *> pure 10
                   , At.string "Nov" *> At.skipSpace *> pure 11
                   , At.string "Dec" *> At.skipSpace *> pure 12
                   ]
