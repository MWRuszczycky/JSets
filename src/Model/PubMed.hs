{-# LANGUAGE OverloadedStrings #-}

module Model.PubMed
    ( eUtilsUrl
    , eSearchUrl
    , eSummaryUrl
    , tocESearchQuery
    , tocESumQuery
    ) where

import qualified Data.Text            as Tx
import qualified Model.Core.Core      as C
import qualified Model.Core.Types     as T
import qualified Network.Wreq         as Wreq
import           Data.Text                    ( Text      )
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

eUtilsUrl :: String
eUtilsUrl = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"

eSearchUrl :: String
eSearchUrl = eUtilsUrl <> "esearch.fcgi?db=pubmed"

eSummaryUrl :: String
eSummaryUrl = eUtilsUrl <> "esummary.fcgi?db=pubmed"

eSearchTerm :: T.HasIssue a => a -> Text
eSearchTerm x = Tx.intercalate " AND " keys
    where keys = [ "\"" <> ( T.pubmed . T.journal) x <> "\"[journal]"
                 , ( C.tshow . T.year  ) x <> "[ppdat]"
                 , ( C.tshow . T.issNo ) x <> "[issue]"
                 , "journal article[pt]"
                 ]

tocESearchQuery :: T.HasIssue a => a -> Wreq.Options
-- ^Build an E-Utilities query to obtain the PMIDs for all citations
-- in the PubMed database associated with the given journal issue.
tocESearchQuery x = Wreq.defaults & Wreq.param "retmode" .~ [ "json"        ]
                                  & Wreq.param "retmax"  .~ [ "200"         ]
                                  & Wreq.param "term"    .~ [ eSearchTerm x ]

tocESumQuery :: [Text] -> Wreq.Options
-- ^Build an E-Utilities query to obtain the document summaries from
-- the PubMed database for the indicated PMIDs.
tocESumQuery pmids = let idstr = Tx.intercalate "," pmids
                     in  Wreq.defaults & Wreq.param "retmode" .~ [ "json" ]
                                       & Wreq.param "id"      .~ [ idstr  ]
