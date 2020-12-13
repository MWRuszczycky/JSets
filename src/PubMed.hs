{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module PubMed
    ( -- URLs
      eUtilsUrl
    , eSearchUrl
    , eSummaryUrl
      -- Constructing web requests to query PubMed
    , eSearchTerm
    , eSearchQuery
    , eSummaryQuery
      -- PubMed web interface: helper functions
    , getWreqSession
    , delayMapM
    , handleMissingCitations
      -- PubMed web interface: eSearch & PMID requests
    , eSearch
    , getPMIDs
      -- PubMed web interface: eSummary & citation requests
    , eSummary
    , getCitations
      -- PubMed web interface: tables of contents
    , getToC
    , getToCs
    ) where

import qualified AppMonad             as A
import qualified Data.Text            as Tx
import qualified Data.Map.Strict      as Map
import qualified Model.Core.Core      as C
import qualified Model.Core.CoreIO    as C
import qualified Model.Core.Dates     as D
import qualified Model.Core.Types     as T
import qualified Model.Journals       as J
import qualified Model.Parsers.PubMed as P
import qualified Network.Wreq         as Wreq
import qualified View.View            as V
import qualified View.Core            as Vc
import           Data.Text                    ( Text         )
import           Data.List                    ( nub          )
import           Lens.Micro                   ( (.~), (&)    )
import           Network.Wreq.Session         ( newSession   )
import           Control.Monad.Reader         ( asks, when   )
import           Control.Monad.Except         ( liftIO, lift
                                              , throwError
                                              , runExceptT   )

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
-- Constructing web requests to query PubMed

---------------------------------------------------------------------
-- Terms & options for construction of ESearch requests to get PMIDs

eSearchTerm :: T.CanQuery a => a -> T.ESearchTerm
eSearchTerm = Tx.intercalate " AND " . map go . T.query
    where go (T.TitleQry   x) = "\"" <> x <> "\"[title]"
          go (T.PageQry    x) = C.tshow x <> "[page]"
          go (T.DOIQry     x) = x         <> "[doi]"
          go (T.JournalQry x) = "\"" <> x <> "\"[journal]"
          go (T.WildQry    x) = "\"" <> x <> "\"[ALL fields]"
          go (T.PMIDQry    x) = x         <> "[pmid]"
          go (T.YearQry    x) = C.tshow x <> "[ppdat]"
          go (T.NumberQry  x) = C.tshow x <> "[issue]"
          go (T.VolumeQry  x) = C.tshow x <> "[volume]"

eSearchQuery :: T.CanQuery a => a -> T.AppMonad Wreq.Options
eSearchQuery x = do
    let maxResults = C.tshow 200
    pure $ Wreq.defaults & Wreq.param "retmode".~ [ "json"        ]
                         & Wreq.param "retmax" .~ [ maxResults    ]
                         & Wreq.param "term"   .~ [ eSearchTerm x ]

---------------------------------------------------------------------
-- Options for construction of ESummary requests to get citations

eSummaryQuery :: [T.PMID] -> Wreq.Options
-- ^Build an E-Utilities query to obtain the document summaries from
-- the PubMed for the indicated PMIDs. These provide citations.
eSummaryQuery pmids = let idstr = Tx.intercalate "," pmids
                      in  Wreq.defaults & Wreq.param "retmode" .~ [ "json" ]
                                        & Wreq.param "id"      .~ [ idstr  ]

-- =============================================================== --
-- PubMed web interface: AppMonad functions

---------------------------------------------------------------------
-- Helper functions for making PubMed requests

getWreqSession :: T.AppMonad C.WebRequest
-- ^Create a new Wreq session that can be used between requests to
-- PubMed to make them more efficient.
getWreqSession = C.webRequestIn <$> liftIO newSession

delayMapM :: (a -> T.AppMonad b) -> [a] -> T.AppMonad [b]
-- ^PubMed only allows 3 requests per second or you risk getting your
-- IP address bloced. This function is the same as mapM with the
-- AppMonad; however, it intersperses a minimum 1 second delay after
-- every two AppMonad actions sequenced in the list. This ensures
-- that no more than 2 requests are sent to PubMed per second.
delayMapM _ [] = pure []
delayMapM f xs = do
    let (ys, rest) = splitAt 2 xs
    us <- mapM f ys
    when (not . null $ rest) delay
    vs <- delayMapM f rest
    pure $ us <> vs

delay :: T.AppMonad ()
-- ^Cause a delay of at least 1 second in a sequenced AppMonad action.
-- The delay is specified by the cDelay configuration value.
delay = do d <- asks $ (* 10^12) . T.cDelay
           C.putTxtMIO $ "Delay " <> Vc.showPicoSec d <> " between requests.."
           liftIO . D.wait $ d
           C.putTxtMIO "\ESC[2K\ESC[0G"

---------------------------------------------------------------------
-- Handler functions for missing PMIDs and citations

handleMissingCitations :: [T.PMID] -> T.AppMonad ()
-- ^Handler for missing PMIDs in ESummary requests that do not return
-- a citation from PubMed.
handleMissingCitations [] = C.putTxtLnMIO "No missing citations."
handleMissingCitations _  = C.putTxtLnMIO "There were missing citations!"

---------------------------------------------------------------------
-- Downloading PMIDs via ESearch requests

eSearch :: T.CanQuery a =>
           C.WebRequest -> a -> T.AppMonad (Either T.ErrString Text)
-- ^Submit an ESearch request to PubMed for the given queriable
-- value. The ESearch response is returned as json.
eSearch wreq x = do
    query <- eSearchQuery x
    liftIO . runExceptT . wreq query $ eSearchUrl

getPMIDs :: T.CanQuery a => C.WebRequest -> a -> T.AppMonad [T.PMID]
-- ^Submit ESearch request to PubMed for a queriable value and parse
-- the resulting json response to PubMed IDs.
getPMIDs wreq x = do
    result <- eSearch wreq x
    case result >>= P.parsePMIDs of
         Left _   -> C.putTxtLnMIO "Failed!" *> pure []
         Right ps -> pure ps

getSelection :: C.WebRequest -> T.Selection -> T.AppMonad [T.Selection]
-- ^Convert selections to PMIDs. If not already provided as PMID, the
-- selection is used to query PubMed via an eSearch request.
getSelection _    (T.ByBndPMID  i p) = pure [T.ByBndPMID i p]
getSelection _    (T.ByPMID       p) = pure [T.ByPMID      p]
getSelection _    (T.ByLink       l) = throwError $ "Unresolved: " <> Tx.unpack l
getSelection wreq x@(T.ByBndDOI i _) = map (T.ByBndPMID i) <$> getPMIDs wreq x
getSelection wreq x                  = map T.ByPMID        <$> getPMIDs wreq x

getOneSelection :: C.WebRequest -> T.Selection -> T.AppMonad [T.Selection]
-- ^Same as getSelection, but allow no more than one PMID.
getOneSelection wreq x = do
    let noneMsg = "No PMID found for " <> show x
        manyMsg = "Multiple PMIDs found for " <> show x <> ", skipping."
    getSelection wreq x >>= \case
        []   -> C.putStrLnMIO noneMsg *> pure []
        x:[] -> pure [x]
        _    -> C.putStrLnMIO manyMsg *> pure []

---------------------------------------------------------------------
-- Downloading Citations via ESummary requests

eSummary :: C.WebRequest -> [T.PMID] -> T.AppMonad (Either T.ErrString Text)
-- ^Submit an ESummary request to PubMed with given list of PMIDs.
-- The document summaries are returned as json and describe the
-- associated citations indexed by the PMIDs.
eSummary wreq pmids =
    liftIO . runExceptT . wreq (eSummaryQuery pmids) $ eSummaryUrl

getCitations :: C.WebRequest -> [T.PMID] -> T.AppMonad T.Citations
-- ^Download the citations associated with a list of PMIDs. This is
-- the interface function for placing ESummary requests where the
-- citations are desired but not the json. It wraps the
-- downloadCitations function.
getCitations wreq pmids = do
    (ms,cs) <- downloadCitations wreq . zip [1..] $ pmids
    handleMissingCitations ms
    pure cs

downloadCitations :: C.WebRequest -> [(Int, T.PMID)]
                     -> T.AppMonad ([T.PMID], T.Citations)
-- ^Core function for placing ESummary requests. Missing PMIDs that
-- are requested but cannot be found are also returned.
downloadCitations _    []    = pure ([], Map.empty)
downloadCitations wreq pmids = do
    let (ns,ps) = unzip pmids
        (x0,xn) = (C.tshow . head $ ns, C.tshow . last $ ns)
    C.putTxtMIO $ "Downloading citations " <> x0 <> "-" <> xn <> "..."
    (t, result) <- C.timeIt $ eSummary wreq ps
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
    case result >>= P.parseCitations ps of
         Left  err     -> do C.putTxtLnMIO $ "Failed " <> timeMsg
                             lift . C.writeFileErr "jsets-error.log" . Tx.pack $ err
                             pure ( [], Map.empty )
         Right (ms,cs) -> do C.putTxtLnMIO $ "OK " <> timeMsg
                             pure ( ms, cs        )

---------------------------------------------------------------------
-- Downloading issue content

getToC :: C.WebRequest -> T.Issue -> T.AppMonad T.ToC
-- ^Get the all citations asssociated with a given journal issue and
-- return the corresponding ToC.
getToC wreq iss = do
    C.putTxtMIO $ "Downloading PMIDs for " <> V.showIssue iss <> "..."
    (t, result) <- C.timeIt $ eSearch wreq iss
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
    case result >>= P.parsePMIDs of
         Left  _     -> do C.putTxtLnMIO $ "Failed " <> timeMsg
                           pure ( T.ToC iss Tx.empty [] )
         Right []    -> do C.putTxtLnMIO $ "No PMIDs " <> timeMsg
                           handleMissingContent iss
         Right pmids -> do C.putTxtLnMIO $ "OK " <> timeMsg
                           pure $ T.ToC iss Tx.empty pmids

handleMissingContent :: T.Issue -> T.AppMonad T.ToC
-- ^Handler for the event that the ToC of a given journal issue
-- cannot be found at PMID. This allows the user to enter an alternate
-- url to the ToC at the publisher's website if available.
handleMissingContent iss = do
    C.putTxtLnMIO $ "  No articles were found at PubMed for " <> V.showIssue iss
    C.putTxtLnMIO $ "  Enter an alternate URL or just press enter to continue:"
    C.putTxtMIO   $ "    https://"
    url <- Tx.strip . Tx.pack <$> liftIO getLine
    pure $ T.ToC iss url []

getToCs :: T.JSet T.Issue -> T.AppMonad (T.Citations, T.JSet T.ToC)
-- ^Request tables of contents for a Journal Set and the citations.
-- Delays are used to avoid exceeding the request limit at PubMed.
getToCs (T.JSet n issues sel) = do
    wreq <- getWreqSession
    -- First, the PMIDs from the user selection. Some of these may
    -- require an eSearch query to PubMed. The PMIDs are wrapped as
    -- Selections in order to bind them to issues as necessary.
    C.putTxtLnMIO "Resolving selection.."
    let (wID, woID) = C.splitOn J.isPMID sel
    selIDs <- nub . (<> wID) . concat <$> delayMapM (getOneSelection wreq) woID
    -- Get all the PMIDs associated with each issue's ToC. We need an
    -- extra delay here to ensure that the last two requests are not
    -- in the same second of time as the next two.
    delay
    C.putTxtLnMIO "Requesting PMIDs per issue (eSearch).."
    xs <- delayMapM (getToC wreq) issues
    let pmids = zip [1..] . C.addUnique (J.pmidsInSelection selIDs)
                          . concatMap T.contents $ xs
    -- Once we have all the PMIDs, we can place the eSummary requests
    -- to get the associated citations. However, PubMed will not allow
    -- too many eSummary PMIDs to be queried in a single request. So,
    -- we have to break up the requests into chunks.
    -- (Still need to figure out what the PMID limit per request is.)
    delay
    C.putTxtLnMIO $ "There are " <> C.tshow (length pmids) <> " PMIDs:"
    (ms,cs) <- fmap unzip . delayMapM (downloadCitations wreq)
                          . C.chunksOf 100 $ pmids
    -- Clean up: 1. Inform user of any missing citations.
    handleMissingCitations . concat $ ms
    -- 2. Correct parsed issues with configured issues if possible.
    rcs <- mapM (A.resolveIssueWith issues) . mconcat $ cs
    -- 3. Update ToCs with any PMIDs that were user-specified.
    --    This may be required if the issue itself is not indexed at
    --    PubMed, but the citations are present as 'ahead of print'.
    let xsFinal = map (J.updateContent selIDs) xs
    pure ( rcs, T.JSet n xsFinal selIDs )