{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module AppMonad
    ( -- Data structer construction & aquisition
      readJSets
    , getJSet
    , getJSets
      -- Working with configured reference issues
    , references
    , getIssue
      -- Internet requests
    , downloadPMIDs
    , downloadCitations
    , downloadContent
    , downloadToCs
      -- Running rank matchings
    , runMatch
    ) where

import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as Tx
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Dates          as D
import qualified Model.Core.Types          as T
import qualified Model.Journals            as J
import qualified Model.Parsers.JournalSets as P
import qualified Model.Parsers.PubMed      as P
import qualified Model.PubMed              as PM
import qualified View.Core                 as Vc
import qualified View.View                 as V
import           Data.Text                          ( Text           )
import           Network.Wreq.Session               ( newSession     )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , runExceptT
                                                    , lift
                                                    , throwError     )

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

readJSets :: FilePath -> T.AppMonad (T.JSets T.Issue)
-- ^Obtain a collection of journal sets from a file.
readJSets fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseJSets refs content of
         Left err    -> throwError err
         Right jsets -> pure jsets

getJSet :: T.HasIssue a => T.JSets a -> Maybe Int -> T.AppMonad (T.JSet a)
-- ^Find a single journal set from a collection of journal sets
-- consistent with any journal set key provided. If no key is
-- provided and there is only journal set in the collection then
-- return that single journal set.
getJSet (T.JSets []) _ = throwError "No journal sets!"
getJSet (T.JSets (j:[])) (Just k)
    | k == T.setNo j = pure j
    | otherwise      = throwError "Journal set does not match requested key!"
getJSet (T.JSets (j:[])) Nothing  = pure j
getJSet _     Nothing  = throwError "A valid journal set key is required!"
getJSet jsets (Just k) =
    let cannotFind = "Cannot find requested journal set!"
    in  case J.lookupJSet k jsets of
             Nothing   -> throwError cannotFind
             Just jset -> pure jset

getJSets :: T.HasIssue a => T.JSets a -> Maybe Int -> T.AppMonad (T.JSets a)
-- ^This is the same as getJSet, but returns a JSets collection. If
-- a key is provided, then the collection is restricted to the single
-- requested journal set. Otherwise, all the journal sets are returned.
getJSets (T.JSets []) _ = throwError "No journal sets!"
getJSets (T.JSets (j:[])) (Just k)
    | k == T.setNo j = pure . J.pack $ [j]
    | otherwise      = throwError "Journal set does not match requested key!"
getJSets (T.JSets (j:[])) Nothing  = pure . J.pack $ [j]
getJSets jsets Nothing  = pure jsets
getJSets jsets (Just k) =
    let cannotFind = "Cannot find requested journal set!"
    in  case J.lookupJSet k jsets of
             Nothing   -> throwError cannotFind
             Just jset -> pure . J.pack $ [jset]

-- =============================================================== --
-- Working with configured reference issues

references :: T.AppMonad T.References
references = asks T.cReferences

getIssue :: Text -> Int -> Int -> T.AppMonad T.Issue
getIssue abbr v n = references >>= maybe err pure . go
    where issMsg = Tx.unpack abbr <> " " <> show v <> ":" <> show n
          err    = throwError $ "Invalid issue: " <> issMsg
          go rs  = J.lookupIssue rs abbr (v,n)

-- =============================================================== --
-- PubMed Pipeline

---------------------------------------------------------------------
-- Core pipeline functions

downloadPMIDs :: C.WebRequest -> T.Issue -> T.AppMonad [T.PMID]
downloadPMIDs wreq iss = do
    let query = PM.tocESearchQuery iss
    C.putTxtMIO $ "Downloading " <> V.showIssue iss <> " PMIDs..."
    result <- liftIO . runExceptT . wreq query $ PM.eSearchUrl
    case result >>= P.parsePMIDs of
         Left err    -> C.putStrMIO err                    *> pure []
         Right []    -> C.putStrMIO "None found at PubMed" *> pure []
         Right pmids -> C.putStrMIO "OK "                  *> pure pmids

downloadCitations :: C.WebRequest -> Maybe T.Issue -> [T.PMID]
                     -> T.AppMonad T.Citations
downloadCitations _    _     []    = pure Map.empty
downloadCitations wreq mbIss pmids = do
    C.putTxtMIO "Downloading Citations..."
    let query = PM.tocESumQuery pmids
    result <- liftIO . runExceptT . wreq query $ PM.eSummaryUrl
    case result >>= P.parseCitations (T.issue <$> mbIss) pmids of
         Left  err     -> C.putStrMIO err  *> pure Map.empty
         Right ([],cs) -> C.putStrMIO "OK" *> pure cs
         Right (ms,cs) -> let msg = Tx.unwords $ "Missing PMIDS:" : ms
                          in  C.putTxtMIO msg *> pure cs

---------------------------------------------------------------------
-- Downloading issue content

downloadContent :: C.WebRequest -> T.Issue -> T.AppMonad (T.Citations, T.Content)
downloadContent wreq iss = do
    start <- liftIO D.readClock
    pmids <- downloadPMIDs wreq iss
    cites <- downloadCitations wreq (Just iss) pmids
    delta <- liftIO . D.deltaClock $ start
    C.putTxtLnMIO $ " (" <> Vc.showPicoSec delta <> ")"
    -- Delay by at least one second or risk being cutoff by PubMed.
    delay <- asks T.cDelay
    liftIO . D.wait $ delay * 10^12
    if null pmids
       then handleMissingPMIDs iss
       else pure $ (cites, T.Content iss Tx.empty pmids)

downloadToCs :: [T.Issue] -> T.AppMonad (T.Citations, [T.Content])
-- ^PubMed has a limit on the length of a request. Too many PMIDs
-- requested will generate a 414 error. So, it is best to request the
-- citations for each issue separately then combine them rather than
-- collecting all the PMIDs for all issues into a single request.
downloadToCs xs = do
    wreq          <- C.webRequestIn <$> liftIO newSession
    (cites,conts) <- unzip <$> mapM (downloadContent wreq) xs
    C.putTxtLnMIO "Done"
    pure (mconcat cites, conts)

handleMissingPMIDs :: T.Issue -> T.AppMonad (T.Citations, T.Content)
handleMissingPMIDs iss = do
    C.putTxtLnMIO $ "  No articles were found at PubMed for " <> V.showIssue iss
    C.putTxtLnMIO $ "  Enter an alternate URL or just press enter to continue:"
    C.putTxtMIO   $ "    https://"
    url <- Tx.strip . Tx.pack <$> liftIO getLine
    pure $ (Map.empty, T.Content iss url [])

-- =============================================================== --
-- Rank matching

runMatch :: [(Text, [[Int]])] -> (Text, [Int]) -> T.AppMonad T.MatchResult
runMatch ranklists (title, indices) = pure $ J.match title indices ranklists
