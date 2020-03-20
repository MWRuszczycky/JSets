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
      -- Internet requests
    , downloadIssueToc
      -- General helper functions
    , getFormat
    , requireKey
    ) where

import qualified Data.Text.IO              as Tx
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Core           as C
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified Model.Core.References     as R
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

jsetsFromFile :: FilePath -> T.AppMonad (T.Result T.JournalSets)
jsetsFromFile fp = lift ( C.readFileErr fp )
                   >>= liftEither . P.parseJsets
                   >>= pure . T.Result R.issueRefKeys

---------------------------------------------------------------------
-- Read a single journal set from a file

jsetFromFile :: FilePath -> T.AppMonad (T.Result T.JournalSet)
-- ^Get a journal set based on the configuration.
jsetFromFile fp = do
    jsets <- T.result <$> jsetsFromFile fp
    key   <- requireKey
    case J.lookupJSet key jsets of
         Nothing   -> throwError "Cannot find requested journal set."
         Just jset -> pure $ T.Result R.issueRefKeys jset

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
    mbFmt <- asks T.cFormat
    path  <- asks T.cOutputPath
    case ( mbFmt, C.extension <$> path ) of
         (Just fmt, _         ) -> pure fmt
         (Nothing , Just "txt") -> pure T.TXT
         (Nothing , Just "md" ) -> pure T.MKD
         (Nothing , Just "mkd") -> pure T.MKD
         (Nothing , Just "csv") -> pure T.CSV
         _                      -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
