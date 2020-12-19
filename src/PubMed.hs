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
import           Control.Monad.Reader         ( when         )
import           Control.Monad.Except         ( liftIO
                                              , runExceptT   )

-- =============================================================== --
-- PubMed interface
-- Queries of the PubMed data base are in two stages. The first uses
-- the ESearch to obtain the unique ID (a.k.a., UID, PubMed ID or
-- PMID) numbers of the requested information, which here each is the
-- article citation. Once obtained, the PMIDs need to be submitted
-- in a second request to PubMed in order to acquire the actual
-- citation information as a document summary. This is done using the
-- ESummary E-utility. For more information, see
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
    pure $ Wreq.defaults & Wreq.param "retmode" .~ [ "json"        ]
                         & Wreq.param "retmax"  .~ [ maxResults    ]
                         & Wreq.param "term"    .~ [ eSearchTerm x ]

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
    when (not . null $ rest) A.delay
    vs <- delayMapM f rest
    pure $ us <> vs

---------------------------------------------------------------------
-- Downloading PMIDs via ESearch requests

eSearch :: T.CanQuery a =>
           C.WebRequest -> a -> T.AppMonad (Either T.ErrString Text)
-- ^Submit an ESearch request to PubMed for the given queriable
-- value. The ESearch response is returned as json. This function
-- not throw any exceptions and instead returns the result as an
-- Either value. It also does not generate any messages.
eSearch wreq x = do
    query <- eSearchQuery x
    liftIO . runExceptT . wreq query $ eSearchUrl

getPMIDs :: T.CanQuery a => C.WebRequest -> a -> T.AppMonad [T.PMID]
-- ^Submit ESearch request to PubMed for a queriable value and parse
-- the resulting json response to PubMed IDs. This function does not
-- throw any exceptions. If the PubMed request cannot be completed or
-- the returned json cannot be parsed, then an error message is
-- generated and the error details are logged.
getPMIDs wreq x = do
    result <- eSearch wreq x
    case result >>= P.parsePMIDs of
         Right ps  -> pure ps
         Left  err -> let msg = "Failed!"
                          hdr = "Unable to download or parse eSearch:"
                      in  (A.logError msg hdr . Tx.pack) err
                          *> pure []

---------------------------------------------------------------------
-- Downloading Citations via ESummary requests

eSummary :: C.WebRequest -> [T.PMID] -> T.AppMonad (Either T.ErrString Text)
-- ^Submit an ESummary request to PubMed with given list of PMIDs.
-- The document summaries are returned as json and describe the
-- associated citations indexed by the PMIDs. This function does not
-- throw any exceptions and returns the result as an Either value. It
-- also does not generate any messages.
eSummary wreq pmids =
    liftIO . runExceptT . wreq (eSummaryQuery pmids) $ eSummaryUrl

getCitations :: C.WebRequest -> [T.PMID] -> T.AppMonad T.Citations
-- ^Submit an ESummary request to PubMed for the requested PMIDs and
-- return any corresponding citations. This function does not throw
-- any exceptions. If the request fails, then a error message is
-- produced and the error details are logged. If any PMIDs do not
-- return a citation, then an error message is generated.
getCitations wreq pmids = do
    result <- eSummary wreq pmids
    paintY <- A.getPainter "yellow"
    paintR <- A.getPainter "red"
    case result >>= P.parseCitations pmids of
         Right ([], cs) -> pure cs
         Right (ms, cs) -> let msg = paintY "Missing citations:\n"
                                     <> Tx.unlines ms
                           in  A.logMessage msg *> pure cs
         Left  err      -> let msg = paintR "Failed!"
                               hdr = "Failed to download or parse eSummary:"
                           in  (A.logError msg hdr . Tx.pack) err
                               *> pure Map.empty

-- =============================================================== -- 
-- Special functions for the toc command
--
-- These functions have more error handling options than the regular
-- PubMed query functions to help with constructing toc output. They
-- also need to handle queries from selection files. So, they are
-- more complicated.

getToCs :: T.JSet T.Issue -> T.AppMonad (T.Citations, T.JSet T.ToC)
-- ^Request tables of contents for a Journal Set and the citations.
-- Delays are used to avoid exceeding the request limit at PubMed.
getToCs (T.JSet n issues sel) = do
    wreq <- getWreqSession
    -- First, the PMIDs from the user selection. Some of these may
    -- require an eSearch query to PubMed. The PMIDs are wrapped as
    -- Selections in order to bind them to issues as necessary.
    A.logMessage "Resolving selection...\n"
    let (wID, woID) = C.splitOn J.isPMID sel
    selIDs <- nub . (<> wID) . concat <$> delayMapM (getOneSelection wreq) woID
    -- Get all the PMIDs associated with each issue's ToC. We need an
    -- extra delay here to ensure that the last two requests are not
    -- in the same second of time as the next two.
    A.delay
    A.logMessage "Requesting PMIDs per issue (eSearch)...\n"
    tocs <- delayMapM (getToC wreq) issues
    let pmids = zip [1..] . C.addUnique (J.pmidsInSelection selIDs)
                          . concatMap T.contents $ tocs
    -- Once we have all the PMIDs, we can place the eSummary requests
    -- to get the associated citations. However, PubMed will not allow
    -- too many eSummary PMIDs to be queried in a single request. So,
    -- we have to break up the requests into chunks.
    -- (Still need to figure out what the PMID limit per request is.)
    A.delay
    A.logMessage $ "There are " <> C.tshow (length pmids) <> " PMIDs...\n"
    (missing,cites) <- fmap unzip . delayMapM (downloadCitations wreq)
                                  . C.chunksOf 100 $ pmids
    -- Clean up: 1. Inform user of any missing citations.
    handleMissingCitations . concat $ missing
    -- 2. Update ToCs with any PMIDs that were user-specified.
    -- 3. Correct parsed issues with configured issues if possible.
    rs <- A.references
    let fixedToCs  = map (J.updateToC selIDs) tocs
        fixedCites = Map.map (J.correctCitation rs fixedToCs) . mconcat $ cites
    pure ( fixedCites, T.JSet n fixedToCs selIDs )

getToC :: C.WebRequest -> T.Issue -> T.AppMonad T.ToC
-- ^Get the all citations asssociated with a given journal issue and
-- return the corresponding ToC.
getToC wreq iss = do
    A.logMessage $ "Downloading PMIDs for " <> V.showIssue iss <> "..."
    (t, result) <- C.timeIt $ eSearch wreq iss
    paintG      <- A.getPainter "green"
    paintY      <- A.getPainter "yellow"
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
        errMsg  = "Failed to obtain PMIDs for" <> V.showIssue iss
        missMsg = paintY "Missing PMIDs " <> timeMsg <> "\n"
        okMsg   = paintG "OK " <> timeMsg <> "\n"
    case result >>= P.parsePMIDs of
         Left  err   -> do A.logError "Failed" errMsg $ Tx.pack err
                           pure $ T.ToC iss Tx.empty []
         Right pmids -> do if length pmids < (T.mincount . T.journal) iss
                              then do A.logMessage missMsg
                                      handleMissingPMIDs pmids iss
                              else do A.logMessage okMsg
                                      pure $ T.ToC iss Tx.empty pmids

---------------------------------------------------------------------
-- Resolving selection PMIDs and locators from selection files

getSelection :: C.WebRequest -> T.Selection -> T.AppMonad [T.Selection]
-- ^Convert selections to PMIDs. If not already provided as PMID, the
-- selection is used to query PubMed via an eSearch request.
getSelection _      (T.ByBndPMID i p) = pure [T.ByBndPMID i p]
getSelection _      (T.ByPMID      p) = pure [T.ByPMID      p]
getSelection wreq x@(T.ByBndDOI  i _) = map  (T.ByBndPMID i) <$> getPMIDs wreq x
getSelection wreq x@(T.ByDOI       _) = map   T.ByPMID       <$> getPMIDs wreq x
getSelection _      (T.ByWeb       x) = do
    paint <- A.getPainter "yellow"
    let msg = Tx.concat
              [ "  Cannot resolve web locator: " <> paint x <> "\n"
              , "    Enter PMID or blank to skip: " ]
    A.request msg >>= \case ""   -> pure []
                            pmid -> pure [T.ByPMID pmid]

getOneSelection :: C.WebRequest -> T.Selection -> T.AppMonad [T.Selection]
-- ^Same as getSelection, but allow no more than one PMID.
getOneSelection wreq x = do
    let noneMsg = "No PMID found for " <> C.tshow x <> ", skipping...\n"
    getSelection wreq x >>= \case
        []   -> A.logMessage noneMsg *> pure []
        p:[] -> pure [p]
        ps   -> let manyHdr = "Multiple PMIDs found for " <> C.tshow x
                    manyMsg = manyHdr <> ", skipping..."
                    manyErr = Tx.unlines . J.pmidsInSelection $ ps
                in  A.logError manyMsg manyHdr manyErr
                    *> pure []

----------------------------------------------------------------------
-- Downloading citations for toc output

downloadCitations :: C.WebRequest -> [(Int, T.PMID)]
                     -> T.AppMonad ([T.PMID], T.Citations)
-- ^Core function for placing ESummary requests. Missing PMIDs that
-- are requested but cannot be found are also returned.
downloadCitations _    []    = pure ([], Map.empty)
downloadCitations wreq pmids = do
    let (ns,ps) = unzip pmids
        (x0,xn) = (C.tshow . head $ ns, C.tshow . last $ ns)
    A.logMessage $ "Downloading citations " <> x0 <> "-" <> xn <> "..."
    (t, result) <- C.timeIt $ eSummary wreq ps
    paint       <- A.getPainter "green"
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
        errMsg  = "Failed to download or parse eSummary"
    case result >>= P.parseCitations ps of
         Right (ms,cs) -> do A.logMessage $ paint "OK " <> timeMsg <> "\n"
                             pure ( ms, cs )
         Left  err     -> do A.logError "Failed" errMsg $ Tx.pack err
                             pure ( [], Map.empty )

---------------------------------------------------------------------
-- Handler and checker functions for when there may be problems with
-- the toc requests

handleMissingPMIDs :: [T.PMID] -> T.Issue -> T.AppMonad T.ToC
-- ^Handler for the event that the ToC of a given journal issue
-- cannot be found at PubMed. This allows the user to enter an
-- alternate url to the ToC at the publisher's website if available.
handleMissingPMIDs pmids iss = do
    paint <- A.getPainter "yellow"
    let n    = paint . C.tshow . length $ pmids
        m    = paint . C.tshow . T.mincount . T.journal $ iss
        name = paint . V.showIssue $ iss
        msg  = Tx.concat
                  [ "  There were " <> n <> " articles found at PubMed for "
                  , name <> "\n  However, at least " <> m <> " were expected."
                  , "\n  Enter an alternate ToC URL or press <enter> to skip:\n"
                  , "    https://" ]
    url <- A.request msg
    pure $ T.ToC iss url pmids

handleMissingCitations :: [T.PMID] -> T.AppMonad ()
-- ^Handler for PMIDs that do not return a citation from PubMed.
handleMissingCitations [] = do
    paint <- A.getPainter "green"
    A.logMessage $ paint "No missing citations.\n"
handleMissingCitations ms =
    A.logError "There were missing citations!"
               "The following PMIDs were requested but not found:"
               $ Tx.unlines ms
