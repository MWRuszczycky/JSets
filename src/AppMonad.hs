{-# LANGUAGE OverloadedStrings #-}

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

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Data.Time                 as Tm
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Core           as C
import qualified Model.Journals            as J
import qualified Model.Text.Formatting     as F
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import           Data.Text                          ( Text           )
import           Data.List                          ( find           )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , runExceptT
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

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

downloadPMIDs :: T.HasIssue a => a -> T.AppMonad [Text]
downloadPMIDs iss = do
    let query = J.tocESearchQuery iss
    liftIO . Tx.putStr $ "Downloading " <> F.issueToTxt iss <> " PMIDs..."
    liftIO . hFlush $ stdout
    result <- liftIO . runExceptT $ C.webRequest query J.eSearchUrl
    case result >>= P.parsePMIDs of
         Left err  -> (liftIO . putStrLn) err *> pure []
         Right []  -> (liftIO . putStrLn) "None found at PubMed." *> pure []
         Right ids -> do liftIO . putStr $ "OK, Downloading Citations..."
                         liftIO . hFlush $ stdout
                         pure ids

downloadCitations :: [Text] -> T.AppMonad [T.Citation]
downloadCitations []    = pure []
downloadCitations pmids = do
    let query = J.tocESumQuery pmids
    result <- liftIO . runExceptT $ C.webRequest query J.eSummaryUrl
    case result >>= P.parseCitations of
         Left  err     -> (liftIO . putStrLn) err     *> pure []
         Right ([],cs) -> (liftIO . putStrLn) "Done." *> pure cs
         Right (ms,cs) -> let msg = Tx.unwords $ "Missing PMIDS:" : ms
                          in  (liftIO . Tx.putStrLn) msg *> pure cs

downloadPubMed :: T.Selection -> T.AppMonad T.IssueContent
downloadPubMed sel = downloadPMIDs sel
                     >>= downloadCitations
                     >>= pure . T.IssueContent sel

-- =============================================================== --
-- General Helper Commands

getFormat :: T.AppMonad T.Format
getFormat = do
    path <- asks T.cOutputPath
    case C.extension <$> path of
         Just "txt"  -> pure T.TXT
         Just "csv"  -> pure T.CSV
         Just "html" -> pure T.HTML
         Just "mkd"  -> pure T.MKD
         Just "md"   -> pure T.MKD
         _           -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
