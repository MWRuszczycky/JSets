{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.PubMed
    ( parseCitations
    , parsePMIDs
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Data.Text            as Tx
import qualified Model.Parsers.JSON   as JS
import qualified Model.Parsers.Core   as P
import qualified Data.Attoparsec.Text as At
import           Data.Text                    ( Text       )
import           Data.List                    ( sortBy     )
import           Data.Ord                     ( comparing  )
import           Control.Applicative          ( many       )

-- =============================================================== --
-- Parsing PubMed Entrez ESearch results

parsePMIDs :: Text -> Either T.ErrString [T.PMID]
parsePMIDs json = JS.parse json >>= maybe err pure . go
    where err = Left "Unable to read idlist from PMID request!"
          go  = JS.lookupListWith [ "esearchresult", "idlist" ] JS.str

-- =============================================================== --
-- Parsing PubMed Entrez ESummary results

parseCitations :: Text -> Either T.ErrString ([T.PMID], [T.Citation])
-- ^Parse esummary json from PubMed to construct citations. If any
-- pmids are indexed in the json but do not have corresponding
-- document summaries, these pmids are also returned; however, this
-- should never happen unless PubMed is not working correctly.
parseCitations txt = do
    json  <- JS.parse txt
    pmids <- getResultIDs json
    let (cs, missing) = C.splitMaybe . map (getCitation json) $ pmids
    pure (missing, sortBy (comparing T.pages) . snd . unzip $ cs)

---------------------------------------------------------------------
-- Components

getResultIDs :: JS.JSON -> Either T.ErrString [T.PMID]
getResultIDs = maybe err pure . JS.lookupListWith [ "result", "uids" ] JS.str
    where err = Left "Unable to find PMIDs in ESummary JSON."

getCitation :: JS.JSON -> Text -> (T.PMID, Maybe T.Citation)
getCitation json pmid = (pmid, c)
    where c = do doc <- JS.lookupWith [ "result", pmid ] pure json
                 T.Citation <$> JS.lookupWith [ "title" ] JS.str doc
                            <*> getAuthors doc
                            <*> getPages   doc
                            <*> getDoi     doc
                            <*> pure       pmid

getAuthors :: JS.JSON -> Maybe [Text]
getAuthors json = JS.lookupListWith [ "authors" ] go json
    where go = JS.lookupWith [ "name" ] JS.str

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

