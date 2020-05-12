{-# LANGUAGE OverloadedStrings #-}

module AppMonad
    ( -- Data structer construction & aquisition
      getJsets
    , getJset
    , getSelJset
    , readSelJset
    , cite'
    , cite
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

getJsets :: FilePath -> T.AppMonad T.Collection
getJsets fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseCollection refs content of
         Left err    -> throwError err
         Right jsets -> pure jsets

---------------------------------------------------------------------
-- Read a single journal set from a file

getJset :: FilePath -> T.AppMonad (T.JournalSet T.Issue)
-- ^Get a journal set based on the configuration.
getJset fp = do
    jsets <- getJsets fp
    key   <- requireKey
    case J.lookupJSet key jsets of
         Nothing   -> throwError "Cannot find requested journal set."
         Just jset -> pure jset

readSelJset :: FilePath -> T.AppMonad (T.JournalSet T.Selection)
readSelJset fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseSelection refs content of
         Left err  -> throwError err
         Right sel -> pure sel

getSelJset :: FilePath -> T.AppMonad (T.JournalSet T.Selection)
getSelJset fp = do
    style <- asks T.cToCStyle
    case style of
         T.Propose -> J.selectNone <$> getJset fp
         _         -> readSelJset fp

cite' :: T.Selection -> T.AppMonad (Text, T.IssueContent)
cite' sel = do
    raw <- downloadPubMed sel
    toc <- liftEither . P.parseCited sel $ raw
    pure (raw, toc)

cite :: T.Selection -> T.AppMonad T.IssueContent
cite = fmap snd . cite'

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
-- Internet requests

downloadPubMed :: T.HasIssue a => a -> T.AppMonad Text
-- ^Download the table of contents for a journal issue from PubMed.
-- The download format is the raw PubMed text.
-- Display progress messages for each ToC download.
downloadPubMed x = do
    let addr = "https://www.ncbi.nlm.nih.gov/pubmed"
        opts = J.tocQuery x
    liftIO . Tx.putStr $ "Downloading " <> F.issueToTxt x <> " from PubMed..."
    liftIO . hFlush $ stdout
    result      <- lift $ C.webRequest opts addr
    noCitations <- liftEither . P.noCitations $ result
    if noCitations
       then liftIO . Tx.putStrLn $ "No articles listed at PubMed"
       else liftIO . Tx.putStrLn $ "OK"
    pure result

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
         Just "raw"  -> pure T.RAW
         _           -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
