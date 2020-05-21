{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.PubMed
    ( parseCitations
    , parsePMIDs
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Data.Text            as Tx
import qualified Model.Parsers.Basic  as P
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text       )
import           Data.List                   ( sortBy     )
import           Data.Ord                    ( comparing  )
import           Control.Applicative         ( some, many )

-- =============================================================== --
-- Parsing PubMed Entrez ESearch results

parsePMIDs :: Text -> Either T.ErrString [T.PMID]
parsePMIDs json = P.parseJSON json >>= maybe err pure . go
    where err = Left "Unable to read idlist from PMID request!"
          go  = P.lookupArray [ "esearchresult", "idlist" ] P.jstr

-- =============================================================== --
-- Parsing PubMed Entrez ESummary results

parseCitations :: Text -> Either T.ErrString ([T.PMID], [T.Citation])
parseCitations txt = do
    json  <- P.parseJSON txt
    pmids <- getResultIDs json
    let (cs, missing) = C.splitMaybe . map (getCitation json) $ pmids
    pure (missing, sortBy (comparing T.pages) . snd . unzip $ cs)

---------------------------------------------------------------------
-- Components

getResultIDs :: P.JSON -> Either T.ErrString [T.PMID]
getResultIDs = maybe err pure . P.lookupArray [ "result", "uids" ] P.jstr
    where err = Left "Unable to find PMIDs in ESummary JSON."

getCitation :: P.JSON -> Text -> (T.PMID, Maybe T.Citation)
getCitation json pmid = (pmid, c)
    where c = do doc <- P.lookupJson [ "result", pmid ] pure json
                 T.Citation <$> P.lookupJson [ "title" ] P.jstr doc
                            <*> getAuthors doc
                            <*> getPages   doc
                            <*> getDoi     doc
                            <*> pure       pmid

getAuthors :: P.JSON -> Maybe [Text]
getAuthors json = P.lookupArray [ "authors" ] go json
    where go = P.lookupJson [ "name" ] P.jstr

getDoi :: P.JSON -> Maybe Text
getDoi json = P.lookupJson [ "elocationid" ] P.jstr json >>= go
    where prefix = "https://www.doi.org/"
          go x   = case Tx.take 4 x of
                        ""     -> pure Tx.empty
                        "doi:" -> pure . (prefix <>) . Tx.drop 5 $ x
                        _      -> go . Tx.tail $ x

getPages :: P.JSON -> Maybe (T.PageNumber, T.PageNumber)
getPages json = do
    txt <- P.lookupJson [ "pages" ] P.jstr json
    case At.parseOnly parsePageNumbers txt of
         Right ps -> pure ps
         Left  _  -> pure ( T.PageNumber "online" 1 , T.PageNumber "online" 0 )

parsePageNumbers :: At.Parser (T.PageNumber, T.PageNumber)
parsePageNumbers = do
    prefix0 <- many At.letter
    suffix0 <- read <$> some At.digit
    At.char '-'
    prefix1 <- many At.letter
    suffix1 <- read <$> some At.digit
    pure ( T.PageNumber prefix0 suffix0, T.PageNumber prefix1 suffix1)

