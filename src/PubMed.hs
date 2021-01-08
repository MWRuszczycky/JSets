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
    , getCitationsPMID
      -- PubMed web interface: resolving selections
    , tryResolveToPMIDs
      -- PubMed web interface: tables of contents
    , getToC
    , getToCs
    , doiCleanupMsg
      -- Direct DOI requests bypassing PubMed
    , getCitationsDOI
    , getCitationDOI
    ) where

import qualified AppMonad                as A
import qualified Data.Text               as Tx
import qualified Data.Map.Strict         as Map
import qualified Model.Core.Core         as C
import qualified Model.Core.CoreIO       as C
import qualified Model.Core.Types        as T
import qualified Model.Journals          as J
import qualified Model.Parsers.Citations as P
import qualified Network.Wreq            as Wreq
import qualified View.View               as V
import qualified View.Core               as Vc
import           Data.Text                       ( Text       )
import           Data.List                       ( nub        )
import           Data.Maybe                      ( catMaybes  )
import           Lens.Micro                      ( (.~), (&)  )
import           Network.Wreq.Session            ( newSession )
import           Control.Monad.Reader            ( when, asks )
import           Control.Monad.Except            ( liftIO
                                                 , runExceptT )

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
-- ^Generate an ESearch term string. The WildQry essentially lets
-- PubMed decide how to search for the provided query string. Only
-- the journal term seems to work and requires surrounding quotes.
eSearchTerm = Tx.intercalate " AND " . map go . T.query
    where go (T.AuthorQry  x) =         x <> "[author]"
          go (T.TitleQry   x) =         x <> "[title]"
          go (T.PageQry    x) = C.tshow x <> "[page]"
          go (T.DOIQry     x) =         x <> "[doi]"
          go (T.JournalQry x) = "\"" <> x <> "\"[journal]"
          go (T.WildQry    x) =         x
          go (T.PMIDQry    x) =         x <> "[pmid]"
          go (T.YearQry    x) = C.tshow x <> "[ppdat]"
          go (T.NumberQry  x) = C.tshow x <> "[issue]"
          go (T.VolumeQry  x) = C.tshow x <> "[volume]"

eSearchQuery :: T.CanQuery a => a -> T.AppMonad Wreq.Options
eSearchQuery x = do
    maxResults <- C.tshow <$> asks T.cMaxResults
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
-- IP address blocked. This function is the same as mapM with the
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
-- does not throw any exceptions and instead returns the result is an
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

getCitationsPMID :: C.WebRequest -> [T.PMID] -> T.AppMonad T.Citations
-- ^Wrapper for downloadCitations that breaks up the request into
-- chunks to prevent PubMed from complaining about there being too
-- many PMIDs in the request. Also handles informing the user if
-- there are PMIDs requested that did not return a document summary.
-- This function does not throw any exceptions. If the request fails,
-- then a error message is produced and the error details are logged.
getCitationsPMID wreq pmids = do
    size <- asks T.cESumChunkSize
    A.logMessage $ eSummaryMsg (length pmids)
    (missing,cites) <- fmap unzip . delayMapM (downloadCitations wreq)
                                  . C.chunksOf size $ zip [1..] pmids
    handleMissingCitations . concat $ missing
    pure . mconcat $ cites

downloadCitations :: C.WebRequest -> [(Int, T.PMID)]
                     -> T.AppMonad ([T.PMID], T.Citations)
-- ^Core function for placing ESummary requests. The PubMed IDs need
-- to be paired with an index to help inform the user as to the
-- download progress. If any PMIDs are requested but do not return a
-- document summary, then these are returned as 'missing PMIDs'.
-- This function should not be used directl. Instead the getCitations
-- wrapper should be used.
downloadCitations _    []    = pure ([], Map.empty)
downloadCitations wreq pmids = do
    let (ns,ps) = unzip pmids
        (x0,xn) = (C.tshow . head $ ns, C.tshow . last $ ns)
    A.logMessage $ "Downloading citations " <> x0 <> "-" <> xn <> "..."
    (t, result) <- C.timeIt $ eSummary wreq ps
    paintG      <- A.getPainter T.Green
    paintY      <- A.getPainter T.Yellow
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
        errMsg  = "Failed to download or parse eSummary"
        warnMsg = paintY "Missing citations " <> timeMsg <> "\n"
        okMsg   = paintG "OK " <> timeMsg <> "\n"
    case result >>= P.parseCitations ps of
         Right ([],cs) -> A.logMessage okMsg   *> pure ( [], cs )
         Right (ms,cs) -> A.logMessage warnMsg *> pure ( ms, cs )
         Left  err     -> A.logError "Failed" errMsg (Tx.pack err)
                              *> pure ( [], Map.empty )

handleMissingCitations :: [T.PMID] -> T.AppMonad ()
-- ^Handler for PMIDs that do not return a citation from PubMed.
handleMissingCitations [] = do
    paint <- A.getPainter T.Green
    A.logMessage $ paint "ESummary found citations for all PMIDs.\n"
handleMissingCitations ms =
    A.logError "ESummary did not find citations for all PMIDs!"
               "The following PMIDs were requested but not found:"
               $ Tx.unlines ms

eSummaryMsg :: Int -> Text
eSummaryMsg n = Tx.unwords
    [ "ESummary: Requesting document summaries for", C.tshow n, "PMIDs...\n" ]

-- =============================================================== -- 
-- Resolving selection PMIDs and locators from selection files

tryResolveToPMIDs :: C.WebRequest -> [T.Selection] -> T.AppMonad [T.Selection]
-- ^Given a list of selections, attempt to resolve them to PMIDs
-- wrapped as Selections. Anything that cannot be resolved is
-- returned unchanged. This function does not throw exceptions.
tryResolveToPMIDs wreq xs = do
    -- We use this split to prevent excessive delays & improve messaging.
    let (wID, woID) = C.splitOn J.isPMID xs
    A.logMessage $ resolveSelectionMsg (length wID) (length woID)
    delayMapM (tryResolveToPMID wreq) woID >>= pure . nub . (<> wID)

-- ------------------------------------------------------------------
-- Helpers

tryResolveToPMID :: C.WebRequest -> T.Selection -> T.AppMonad T.Selection
-- ^Try to resolve DOI and Web selections to PMID selections using
-- ESearch. PMID selections are returned as is. DOI selections are
-- resolved as specified by resolveDOIToPMID. Web selections are used
-- to query the user for a DOI or PMID.
tryResolveToPMID _    x@(T.ByBndPMID _ _) = pure x
tryResolveToPMID _    x@(T.ByPMID      _) = pure x
tryResolveToPMID wreq (T.ByBndDOI i  d) = do
    resolveDOItoPMID wreq (T.ByBndPMID i) (T.ByBndDOI i) d
tryResolveToPMID wreq (T.ByDOI d) = do
    resolveDOItoPMID wreq T.ByPMID T.ByDOI d
tryResolveToPMID wreq x@(T.ByWeb w) = do
    response <- requestResolutionHelp w
    case Tx.unpack response of
        ""            -> pure x
        '1':'0':'.':_ -> tryResolveToPMID wreq . T.ByDOI $ response
        pmid          -> pure . T.ByPMID . Tx.pack $ pmid

resolveDOItoPMID :: C.WebRequest
                    -> (T.PMID -> T.Selection) -> (T.DOI -> T.Selection)
                    -> T.DOI -> T.AppMonad T.Selection
-- ^Helper function for tryResolveToPMID. Tries to resolve a doi
-- to a PMID using ESearch. If a single PMID is found it is returned
-- as the correspondig PMID selection. If no PMIDs are found, the doi
-- is returned as the corresponding DOI selection. If multiple PMIDs
-- are found, then there is a problem with the doi, since it should
-- uniquely identify a citation, so a Web selection is returned.
resolveDOItoPMID wreq toPMID toDOI doi = do
    A.logMessage $ "Resolving doi " <> doi <> " to PMID..."
    (t, result) <- C.timeIt $ getPMIDs wreq (T.ByDOI doi)
    paintWarn   <- A.getPainter T.Yellow
    paintOK     <- A.getPainter T.Green
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
        okMsg   = paintOK   "OK "      <> timeMsg <> "\n"
        missMsg = paintWarn "No PMID " <> timeMsg <> "\n"
    case result of
         []   -> A.logMessage missMsg *> pure ( toDOI doi )
         p:[] -> A.logMessage okMsg   *> pure ( toPMID p  )
         ps   -> badDOISelectionError doi ( Tx.unlines ps )
                 *> tryResolveToPMID wreq ( T.ByWeb doi   )

requestResolutionHelp :: Text -> T.AppMonad Text
-- ^Request help from the user in resolving a web locator to either a
-- PMID or a DOI.
requestResolutionHelp x = do
    paintY <- A.getPainter T.Yellow
    A.request $ Tx.concat [ "Cannot resolve locator: " <> paintY x <> "\n"
                          , "    Enter PMID, DOI or blank to skip: "
                          ]

badDOISelectionError :: T.DOI -> Text -> T.AppMonad ()
badDOISelectionError doi err = A.logError msg hdr err
    where msg = "Invalid doi: " <> doi
          hdr = "Multiple PMIDs found for doi: " <> doi

resolveSelectionMsg :: Int -> Int -> Text
resolveSelectionMsg nWithID nNoID
    | nNoID == 0 = mainMsg <> "\n"
    | otherwise  = mainMsg <> " " <> nNoIDStr <> " require resolution...\n"
    where mainMsg  = "There are " <> totalStr <> " articles selected..."
          totalStr = C.tshow $ nWithID + nNoID
          nNoIDStr = C.tshow nNoID

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
    -- Try to resolve selection to PMIDs. Some of these may require
    -- an eSearch query to PubMed or input from the user. The PMIDs
    -- are wrapped as Selections to bind them to issues if possible.
    sel' <- tryResolveToPMIDs wreq sel
    -- Not all of the DOIs will always get resolved to PMIDs. So, we
    -- try to find the citations for these DOIs directly.
    let selPMIDs = J.pmidsInSelection sel'
        selDOIs  = J.doisInSelection  sel'
    A.logMessage . doiCleanupMsg $ length selDOIs
    doiCites <- getCitationsDOI wreq selDOIs
    -- Now get the PMIDs associated with each issue. An extra delay
    -- is added to ensure no overlap with previous PubMed requests.
    A.delay
    A.logMessage . tocsESearchMsg $ length issues
    tocs <- delayMapM (getToC wreq) issues
    let pmids = C.addUnique selPMIDs . concatMap T.contents $ tocs
    -- Once we have all the PMIDs, we can place the eSummary requests
    -- to get the associated citations registered at PubMed.
    A.delay
    pmidCites <- getCitationsPMID wreq pmids
    refs      <- A.references
        -- Combine the citations from PubMed and direct DOI lookup.
    let allCites   = Map.union pmidCites doiCites
        -- Update ToCs with any user-specified citations. This may be
        -- necessary if an issue is not properly indexed at PubMed,
        -- but the articles themselves are still registered at PubMed.
        fixedToCs  = map (J.updateToC sel') tocs
        -- Determine whether each citation is in one of the ToCs and
        -- if so correct its issue field from the naively parsed value.
        finalCites = Map.map (J.correctCitation refs fixedToCs) $ allCites
        -- Sort all the ToCs by page number.
        finalToCs  = map (J.sortToC finalCites T.pages) fixedToCs
    pure ( finalCites, T.JSet n finalToCs sel' )

getToC :: C.WebRequest -> T.Issue -> T.AppMonad T.ToC
-- ^Get the all citations asssociated with a given journal issue and
-- return the corresponding ToC.
getToC wreq iss = do
    A.logMessage $ "Downloading PMIDs for " <> V.showIssue iss <> "..."
    (t, result) <- C.timeIt $ eSearch wreq iss
    paintOK     <- A.getPainter T.Green
    paintWarn   <- A.getPainter T.Yellow
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
        errMsg  = "Failed to obtain PMIDs for" <> V.showIssue iss
        missMsg = paintWarn "Missing PMIDs " <> timeMsg <> "\n"
        okMsg   = paintOK   "OK " <> timeMsg <> "\n"
    case result >>= P.parsePMIDs of
         Left  err   -> do A.logError "Failed" errMsg $ Tx.pack err
                           pure $ T.ToC iss Tx.empty []
         Right pmids -> do if length pmids < (T.mincount . T.journal) iss
                              then do A.logMessage missMsg
                                      handleMissingPMIDs pmids iss
                              else do A.logMessage okMsg
                                      pure $ T.ToC iss Tx.empty pmids

---------------------------------------------------------------------
-- Messaging during the tocs download process

tocsESearchMsg :: Int -> Text
tocsESearchMsg 0 = "No issues to request PMIDs for...\n"
tocsESearchMsg 1 = "ESearch: Requesting PMIDs for 1 issue...\n"
tocsESearchMsg n = Tx.unwords
    [ "ESearch: Requesting PMIDs for", C.tshow n, "issues...\n" ]

doiCleanupMsg :: Int -> Text
doiCleanupMsg 0 = "No unresolved DOIs...\n"
doiCleanupMsg 1 = "Attempting to find citation for 1 unresolved DOI...\n"
doiCleanupMsg n = Tx.unwords
    [ "Attempting to find citations for", C.tshow n, "unresolved DOIs...\n" ]

---------------------------------------------------------------------
-- Handler and checker functions for when there may be problems with
-- the toc requests

handleMissingPMIDs :: [T.PMID] -> T.Issue -> T.AppMonad T.ToC
-- ^Handler for the event that the ToC of a given journal issue
-- cannot be found at PubMed. This allows the user to enter an
-- alternate url to the ToC at the publisher's website if available.
handleMissingPMIDs pmids iss = do
    paint <- A.getPainter T.Yellow
    let n    = paint . C.tshow . length $ pmids
        m    = paint . C.tshow . T.mincount . T.journal $ iss
        name = paint . V.showIssue $ iss
        msg  = Tx.concat
                  [ "  " <> n <> " PMID(s) found at PubMed for "
                  , name <> "\n  At least " <> m <> " were expected:"
                  , " Enter a ToC URL or blank to skip,\n"
                  , "    https://" ]
    url <- A.request msg
    pure $ T.ToC iss url pmids

-- =============================================================== -- 
-- Downloading citations by DOI only (no PubMed)
-- DOI citations are downloaded in the Research Information Systems
-- (RIS) format.
-- See https://citation.crosscite.org/docs.html
--     https://www.doi.org/doi_handbook/5_Applications.html
--     https://en.wikipedia.org/wiki/RIS

doiUrl :: String -> String
doiUrl x = "https://doi.org/" <> x

doiQuery :: Wreq.Options
doiQuery = Wreq.defaults & Wreq.header "Accept" .~ [ hdr ]
    where hdr = "application/x-research-info-systems"

getCitationsDOI :: C.WebRequest -> [T.DOI] -> T.AppMonad T.Citations
getCitationsDOI wreq dois = let go = map ( \ c -> (T.pmid c, c) )
                            in  mapM (getCitationDOI wreq) dois
                                >>= pure . Map.fromList . go . catMaybes

getCitationDOI :: C.WebRequest -> T.DOI -> T.AppMonad (Maybe T.Citation)
-- ^Attempt to download and parse a single citation using its doi.
-- This function does not throw exceptions. If the citation cannot be
-- obtained, an error is logged and a Nothing is returned.
getCitationDOI wreq doi = do
    let url = doiUrl . Tx.unpack $ doi
    A.logMessage $ "Direct doi lookup for " <> doi <> "..."
    paintOK <- A.getPainter T.Green
    (t, result) <- C.timeIt $ liftIO . runExceptT . wreq doiQuery $ url
    let timeMsg = "(" <> Vc.showPicoSec t <> ")"
        okMsg   = paintOK "OK " <> timeMsg <> "\n"
    case result >>= P.parseCitationRIS of
         Right c   -> A.logMessage okMsg *> pure (Just c)
         Left  err -> let msg = "Failed"
                          hdr = "Failed doi download or parse: " <> url
                      in  A.logError msg (Tx.pack hdr) (Tx.pack err)
                          *> pure Nothing
