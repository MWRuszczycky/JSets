{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( -- Downloading tables of contents
      getTocs
      -- Displaying journal sets
    , displayJsets
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified Model.Core.References     as R
import           Data.Text                          ( Text           )
import           Text.Read                          ( readMaybe      )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Reader               ( asks, ask      )
import           Control.Monad.Except               ( liftIO
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Acquiring journal sets

---------------------------------------------------------------------
-- Collections of journal sets

getJsets :: T.AppMonad T.JournalSets
-- ^Get the collection of journal sets based on the configuration.
getJsets = ask >>= go
    where err  = "Cannot find journal sets."
          go c = case (T.cInputPath c, T.cJsetsYear c) of
                      (Just fp, _      ) -> jsetsFromFile fp
                      (Nothing, Just y ) -> jsetsFromYear y
                      (Nothing, Nothing) -> throwError err

jsetsFromFile :: FilePath -> T.AppMonad T.JournalSets
-- ^Read a collection of journal sets from a file.
jsetsFromFile fp = lift $ C.readFileErr fp >>= liftEither . P.parseJsets

jsetsFromYear :: String -> T.AppMonad T.JournalSets
-- ^Build the default collection of journal sets based on a year.
-- The year is provided as a string, which will raise an error if
-- it is an invalid year.
jsetsFromYear = maybe err go . readMaybe
    where err  = throwError "Invalid year."
          go y = pure . J.yearly26Sets y $ R.issueRefs

---------------------------------------------------------------------
-- A single journal set

getJset :: T.AppMonad T.JournalSet
-- ^Get a journal set based on the configuration.
getJset = do
    let err = "Cannot find requested journal set."
    jsets <- getJsets
    key   <- getJsetKey
    jset  <- maybe (throwError err) pure . Map.lookup key $ jsets
    pure (key, jset)

getJsetKey :: T.AppMonad (Int, Int)
-- ^Read the journal set key according to the configuration.
getJsetKey = asks go >>= maybe err pure
    where err  = throwError "Journal set key must be YEAR-NUMBER."
          go u = case break (== '-') <$> T.cJsetKey u of
                      Just (y,'-':n) -> (,) <$> readMaybe y <*> readMaybe n
                      _              -> Nothing

-- =============================================================== --
-- Routing output

toOutput :: Text -> T.AppMonad Text
-- ^Route output according to the configuration.
-- If output is to sent to a file, then it is written to the output
-- file as determined by the configuration and an empty Text value is
-- returned. Otherwise, it is wrapped in the AppMonad so it can be
-- accessed for display in the terminal.
toOutput x = asks T.cOutputPath >>= maybe (pure x) go
    where go fp = lift (C.writeFileErr fp x) *> pure Tx.empty

-- =============================================================== --
-- Downloading tables of contents

getTocs :: T.AppMonad Text
-- ^Acquire the tables of contents for all issues in the journal set
-- according to the collection & key specified by the configuration.
-- The tables of contents are returned as Text in the format also
-- specified by the configuration.
getTocs = do
    (k,xs) <- getJset
    fmt    <- asks T.cFormat
    tocs   <- mapM downloadIssueToc xs
    toOutput $ case fmt of
                    T.CSV -> "ToC conversion to csv is not supported."
                    T.MKD -> F.tocsToMkd k tocs
                    T.TXT -> Tx.unlines . map F.tocToTxt $ tocs

downloadIssueToc ::  T.Issue -> T.AppMonad T.TableOfContents
-- ^Download the table of contents for a journal issue from PubMed.
-- Display progress messages for each ToC download.
downloadIssueToc x = do
    let addr = "https://www.ncbi.nlm.nih.gov/pubmed"
        opts = J.tocQuery x
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    toc <- lift $ C.webRequest opts addr >>= liftEither . P.parseToC x
    if null toc
       then liftIO . Tx.putStrLn $ "No articles listed at PubMed"
       else liftIO . Tx.putStrLn $ "OK"
    pure toc

-- =============================================================== --
-- Displaying journal sets

displayJsets :: T.AppMonad Text
-- ^Retrieve the requested journal set based on the configuration and
-- format for output.
displayJsets = do
    let js = map T.journal R.issueRefs
    jsets <- getJsets
    fmt   <- asks T.cFormat
    toOutput $ case fmt of
                    T.CSV -> F.jsetsToCSV js $ jsets
                    T.MKD -> "Conversion to markdown is not yet implemented."
                    T.TXT -> F.jsetsToTxt $ jsets
