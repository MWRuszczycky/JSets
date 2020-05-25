{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module AppMonad
    ( -- Data structer construction & aquisition
      readJsets
    , readJset
    , getJset
      -- Working with configured reference issues
    , isAvailable
    , references
    , refIssue
    , issueRefAbbrs
    , getIssue
      -- Internet requests
    , downloadPubMed
      -- General helper functions
    , getFormat
    , requireKey
    ) where

import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Core           as C
import qualified Model.Core.Dates          as D
import qualified Model.Journals            as J
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified View.View                 as V
import qualified View.Core                 as Vc
import           Network.Wreq.Session               ( newSession     )
import           Data.Text                          ( Text           )
import           Data.List                          ( find           )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , runExceptT
                                                    , lift
                                                    , throwError     )

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

readJsets :: FilePath -> T.AppMonad (T.Collection T.Selection)
readJsets fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseCollection refs content of
         Left err    -> throwError err
         Right jsets -> pure jsets

---------------------------------------------------------------------
-- Read a single journal set from a file

getJset :: T.HasIssue a => T.Collection a -> T.AppMonad (T.JournalSet a)
getJset jsets
    | Map.null jsets      = throwError noJsetsMsg
    | Map.size jsets == 1 = pure . uncurry T.JSet . Map.findMin $ jsets
    | otherwise           = requested
    where noJsetsMsg = "There are no journal sets in the collection!"
          missingKey = "Cannot find requested journal set in this collection!"
          requested  = do key <- requireKey
                          case J.lookupJset key jsets of
                               Nothing   -> throwError missingKey
                               Just jset -> pure jset

readJset :: FilePath -> T.AppMonad (T.JournalSet T.Selection)
-- ^Get a journal set based on the configuration.
readJset fp = readJsets fp >>= getJset

-- =============================================================== --
-- Working with configured reference issues

isAvailable :: Text -> T.AppMonad Bool
-- ^Determine whether a given journal has a reference issue.
isAvailable abbr = issueRefAbbrs >>= pure . elem abbr

references :: T.AppMonad T.References
references = asks T.cReferences

refIssue :: Text -> T.AppMonad (Maybe T.Issue)
-- ^Find a reference issue by its journal abbreviation.
refIssue abbr = references >>= pure . find ( (== abbr) . T.abbr . T.journal )

issueRefAbbrs :: T.AppMonad [Text]
issueRefAbbrs = references >>= pure . map ( T.abbr . T.journal )

getIssue :: Text -> Int -> Int -> T.AppMonad T.Issue
getIssue abbr v n = references >>= maybe err pure . go
    where issMsg = Tx.unpack abbr <> " " <> show v <> ":" <> show n
          err    = throwError $ "Invalid issue: " <> issMsg
          go rs  = J.lookupIssue rs abbr (v,n)

-- =============================================================== --
-- PubMed Pipeline

downloadPMIDs :: T.HasIssue a => C.WebRequest -> a -> T.AppMonad [Text]
downloadPMIDs wreq iss = do
    let query = J.tocESearchQuery iss
    C.putTxtMIO $ "Downloading " <> V.issueToTxt iss <> " PMIDs..."
    result <- liftIO . runExceptT . wreq query $ J.eSearchUrl
    case result >>= P.parsePMIDs of
         Left err  -> C.putStrMIO err                    *> pure []
         Right []  -> C.putStrMIO "None found at PubMed" *> pure []
         Right ids -> C.putStrMIO "OK "                  *> pure ids

downloadCitations :: C.WebRequest -> [Text] -> T.AppMonad [T.Citation]
downloadCitations _    []    = pure []
downloadCitations wreq pmids = do
    C.putTxtMIO "Downloading Citations..."
    let query = J.tocESumQuery pmids
    result <- liftIO . runExceptT . wreq query $ J.eSummaryUrl
    case result >>= P.parseCitations of
         Left  err     -> C.putStrMIO err  *> pure []
         Right ([],cs) -> C.putStrMIO "OK" *> pure cs
         Right (ms,cs) -> let msg = Tx.unwords $ "Missing PMIDS:" : ms
                          in  C.putTxtMIO msg *> pure cs

downloadPubMed :: T.Selection -> T.AppMonad T.IssueContent
downloadPubMed sel = do
    start <- liftIO D.checkClock
    wreq  <- C.webRequestIn <$> liftIO newSession
    cites <- downloadPMIDs wreq sel >>= downloadCitations wreq
    secs  <- fmap Vc.showPicoSec . liftIO . D.stopClock $ start
    C.putTxtLnMIO $ " (" <> secs <> ")"
    pure $ T.IssueContent sel cites

-- =============================================================== --
-- General Helper Commands

getFormat :: T.AppMonad T.Format
getFormat = asks ( fmap C.extension . T.cOutputPath )
            >>= \case Just "txt"  -> pure T.TXT
                      Just "csv"  -> pure T.CSV
                      Just "html" -> pure T.HTML
                      Just "mkd"  -> pure T.MKD
                      Just "md"   -> pure T.MKD
                      _           -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
