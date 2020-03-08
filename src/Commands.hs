{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( -- Handling output
      finish
      -- Commands
    , readJsetOrJsets
    , jsetsFromYear
    , downloadJsetTocs
      -- Data structer construction & aquisition
    , jsetsFromFile
    , jsetFromFile
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
import           Data.Maybe                         ( isJust         )
import           Text.Read                          ( readMaybe      )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Handling output

finish :: F.Formattable a => T.Result a -> T.AppMonad ()
finish (T.Result hdr x) = do
    fmt  <- getFormat
    path <- asks T.cOutputPath
    case path of
         Nothing -> liftIO . Tx.putStrLn . F.format fmt hdr $ x
         Just fp -> lift . C.writeFileErr fp . F.format fmt hdr $ x

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- File format reading and conversion

readJsetOrJsets :: [String] -> T.AppMonad ()
-- ^Read and output a journal set collection or a single journal set
-- depending on whether or not a valid key is provided.
readJsetOrJsets []     = throwError "A path to the journal sets file required!"
readJsetOrJsets (fp:_) = do
    keyProvided <- asks $ isJust . T.cJsetKey
    if keyProvided
       then jsetFromFile fp  >>= finish
       else jsetsFromFile fp >>= finish

---------------------------------------------------------------------
-- Aquire journal set collections by year

jsetsFromYear :: [String] -> T.AppMonad ()
-- ^Build the default collection of journal sets based on a year.
-- The year is provided as a string, which will raise an error if
-- it is an invalid year.
jsetsFromYear []    = throwError "A valid year must be specified!"
jsetsFromYear (x:_) = maybe err go  (readMaybe x) >>= finish
    where err  = throwError "Invalid year."
          go y = pure . T.Result [C.tshow y] . J.yearly26Sets y $ R.issueRefs

---------------------------------------------------------------------
-- Download tables of contents for all issues in a journal set

downloadJsetTocs :: [String] -> T.AppMonad ()
-- ^Acquire the tables of contents for all issues in the journal set
-- according to the collection & key specified by the configuration.
-- The tables of contents are returned as Text in the format also
-- specified by the configuration.
downloadJsetTocs []     = throwError "Path to the journal sets file is needed!"
downloadJsetTocs (fp:_) = do
    jset <- T.result <$> jsetFromFile fp
    tocs <- mapM downloadIssueToc . T.jsIssues $ jset
    finish $ T.Result [C.tshow $ T.jsKey jset]
             $ T.JSetToC (T.jsKey jset) tocs

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

---------------------------------------------------------------------
-- Downloading issue table of contents

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
         (Just fmt, _          ) -> pure fmt
         (Nothing , Just "txt" ) -> pure T.TXT
         (Nothing , Just "md"  ) -> pure T.MKD
         (Nothing , Just "mkd" ) -> pure T.MKD
         (Nothing , Just "csv" ) -> pure T.MKD
         _                       -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
