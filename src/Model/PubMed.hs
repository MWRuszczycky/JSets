{-# LANGUAGE OverloadedStrings #-}

module Model.PubMed
    ( eUtilsUrl
    , eSearchUrl
    , eSummaryUrl
    , eSearchTerm
    , eSearchQuery
    , eSummaryQuery
    ) where

import qualified Data.Text            as Tx
import qualified Model.Core.Core      as C
import qualified Model.Core.Types     as T
import qualified Network.Wreq         as Wreq
import           Lens.Micro                   ( (.~), (&) )

-- =============================================================== --
-- PubMed interface
-- PMC queries of the PubMed data base are in two stages. The first
-- uses the ESearch E-utility to obtain the unique ID (UID) numbers
-- of the requested information (here each article citation).
-- These UIDs need to be submitted in a second request to PMC to
-- acquire the actual citations. This is done using the ESummary
-- E-utility. The following two functions are used to construct query
-- urls for use with ESearch and ESummary.
-- For more information see
-- https://www.ncbi.nlm.nih.gov/books/NBK25501
-- https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESummary
-- https://www.ncbi.nlm.nih.gov/books/NBK25500/#chapter1.Searching_a_Database

-- =============================================================== --
-- URLs

eUtilsUrl :: String
eUtilsUrl = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"

eSearchUrl :: String
eSearchUrl = eUtilsUrl <> "esearch.fcgi?db=pubmed"

eSummaryUrl :: String
eSummaryUrl = eUtilsUrl <> "esummary.fcgi?db=pubmed"

-- =============================================================== --
-- Constructing web requests to query PubMed for PMIDs

eSearchTerm :: T.CanQuery a => a -> T.ESearchTerm
eSearchTerm = Tx.intercalate " AND " . map go . T.query
    where go (T.TitleQry   x) = "\"" <> x <> "\"[title]"
          go (T.PageQry    x) = C.tshow x <> "[page]"
          go (T.DOIQry     x) = x         <> "[doi]"
          go (T.JournalQry x) = "\"" <> x <> "\"[journal]"
          go (T.WildQry    x) = "\"" <> x <> "\"[ALL fields]"
          go (T.YearQry    x) = C.tshow x <> "[ppdat]"
          go (T.NumberQry  x) = C.tshow x <> "[issue]"
          go (T.VolumeQry  x) = C.tshow x <> "[volume]"

eSearchQuery :: T.CanQuery a => a -> T.AppMonad Wreq.Options
eSearchQuery x = do
    let maxResults = C.tshow 200
    pure $ Wreq.defaults & Wreq.param "retmode".~ [ "json"        ]
                         & Wreq.param "retmax" .~ [ maxResults    ]
                         & Wreq.param "term"   .~ [ eSearchTerm x ]

-- =============================================================== --
-- Constructing web requests to query PubMed for document summaries

eSummaryQuery :: [T.PMID] -> Wreq.Options
-- ^Build an E-Utilities query to obtain the document summaries from
-- the PubMed database for the indicated PMIDs.
eSummaryQuery pmids = let idstr = Tx.intercalate "," pmids
                      in  Wreq.defaults & Wreq.param "retmode" .~ [ "json" ]
                                        & Wreq.param "id"      .~ [ idstr  ]
