{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( -- Downloading tables of contents
      getTocs
      -- Displaying journal sets
    , jsetsFromYear
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
import           Text.Read                          ( readMaybe      )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Acquiring journal sets

---------------------------------------------------------------------
-- Helpers

getJsetKey :: T.AppMonad Int
-- ^Read the journal set key according to the configuration.
getJsetKey = asks T.cJsetKey >>= maybe err pure
    where err  = throwError "Journal set key must be a positive integer"

---------------------------------------------------------------------
-- Collections of journal sets

jsetsFromYear :: [String] -> T.AppMonad ([Text], T.JournalSets)
-- ^Build the default collection of journal sets based on a year.
-- The year is provided as a string, which will raise an error if
-- it is an invalid year.
jsetsFromYear []    = throwError "A valid year must be specified!"
jsetsFromYear (x:_) = maybe err go . readMaybe $ x
    where err  = throwError "Invalid year."
          go y = pure . (,) [C.tshow y] . J.yearly26Sets y $ R.issueRefs

---------------------------------------------------------------------
-- A single journal set

jsetFromFile :: FilePath -> T.AppMonad ([Text], T.JournalSet)
-- ^Get a journal set based on the configuration.
jsetFromFile fp = do
    jsets <- lift $ C.readFileErr fp >>= liftEither . P.parseJsets
    key   <- getJsetKey
    let err = "Cannot find requested journal set."
    maybe (throwError err) (pure . (,) []) . J.lookupJSet key $ jsets

-- =============================================================== --
-- Downloading tables of contents

getTocs :: [String] -> T.AppMonad ([Text], T.JournalSetToC)
-- ^Acquire the tables of contents for all issues in the journal set
-- according to the collection & key specified by the configuration.
-- The tables of contents are returned as Text in the format also
-- specified by the configuration.
getTocs []     = throwError "A file path to the journal sets must be supplied!"
getTocs (fp:_) = do
    jset <- snd <$> jsetFromFile fp
    tocs <- mapM downloadIssueToc . T.jsIssues $ jset
    pure . (,) [C.tshow $ T.jsKey jset] . T.JSetToC (T.jsKey jset) $ tocs

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
