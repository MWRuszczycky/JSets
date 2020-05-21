{-# LANGUAGE OverloadedStrings #-}
module Model.Parsers.PubMed
    ( parseCitations
    , parsePMIDs
    ) where

import qualified Model.Core.Types     as T
import qualified Model.Core.Core      as C
import qualified Data.Text            as Tx
import qualified Model.Parsers.Basic  as P
import           Data.Text                   ( Text       )
import           Data.List                   ( sortBy     )
import           Data.Ord                    ( comparing  )

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

getAuthors :: P.JSON -> Maybe Text
getAuthors json = let go = P.lookupJson [ "name" ] P.jstr
                  in  P.lookupArray [ "authors" ] go json
                      >>= pure . Tx.intercalate ", "

getPages :: P.JSON -> Maybe (T.PageNumber, T.PageNumber)
getPages json = P.lookupJson [ "pages" ] P.jstr json >>= pure . parsePageNumbers

getDoi :: P.JSON -> Maybe Text
getDoi json = P.lookupJson [ "elocationid" ] P.jstr json >>= go
    where go x = case Tx.splitAt 5 x of
                      ("doi: ", d) -> pure d
                      _            -> Nothing

parsePageNumbers :: Text -> (T.PageNumber, T.PageNumber)
parsePageNumbers _ = (T.PageNumber "Cat" 0, T.PageNumber "Dog" 1)
