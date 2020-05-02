{-# LANGUAGE OverloadedStrings #-}

module AppMonad
    ( -- Data structer construction & aquisition
      jsetsFromFile
    , jsetFromFile
      -- Working with configured reference issues
    , isAvailable
    , references
    , refIssue
    , issueRefKeys
    , getIssue
      -- Internet requests
    , downloadIssueToc
      -- General helper functions
    , getFormat
    , requireKey
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
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
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

jsetsFromFile :: FilePath -> T.AppMonad T.JournalSets
jsetsFromFile fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseJsets refs content of
         Left err    -> throwError err
         Right jsets -> pure jsets

---------------------------------------------------------------------
-- Read a single journal set from a file

jsetFromFile :: FilePath -> T.AppMonad T.JournalSet
-- ^Get a journal set based on the configuration.
jsetFromFile fp = do
    jsets <- jsetsFromFile fp
    key   <- requireKey
    case J.lookupJSet key jsets of
         Nothing   -> throwError "Cannot find requested journal set."
         Just jset -> pure jset

-- =============================================================== --
-- Working with configured reference issues

isAvailable :: Text -> T.AppMonad Bool
-- ^Determine whether a given journal has a reference issue.
isAvailable key = issueRefKeys >>= pure . elem key

references :: T.AppMonad [T.Issue]
references = asks T.cReferences

refIssue :: Text -> T.AppMonad (Maybe T.Issue)
-- ^Find a reference issue by its journal key.
refIssue key = references >>= pure . find ( (== key) . T.key . T.journal )

issueRefKeys :: T.AppMonad [Text]
issueRefKeys = references >>= pure . map ( T.key . T.journal )

getIssue :: Text -> Int -> Int -> T.AppMonad T.Issue
getIssue key v n = references >>= maybe err pure . go
    where issMsg = Tx.unpack key <> " " <> show v <> ":" <> show n
          err    = throwError $ "Invalid issue: " <> issMsg
          go rs  = J.lookupIssue rs key (v,n)

-- =============================================================== --
-- Internet requests

downloadIssueToc ::  T.Issue -> T.AppMonad T.IssueToC
-- ^Download the table of contents for a journal issue from PubMed.
-- Display progress messages for each ToC download.
downloadIssueToc x = do
    let addr = "https://www.ncbi.nlm.nih.gov/pubmed"
        opts = J.tocQuery x
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    cs <- lift $ C.webRequest opts addr >>= liftEither . P.parseCitations x
    if null cs
       then liftIO . Tx.putStrLn $ "No articles listed at PubMed"
       else liftIO . Tx.putStrLn $ "OK"
    pure $ T.IssueToC x cs

-- =============================================================== --
-- General Helper Commands

getFormat :: T.AppMonad T.Format
getFormat = do
    path <- asks T.cOutputPath
    case C.extension <$> path of
         Just "txt"  -> pure T.TXT
         Just "csv"  -> pure T.CSV
         Just "html" -> pure T.HTML
         _           -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
