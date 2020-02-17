{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( downloadPubMedToc
    , writeTocs
    , convert
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Network.Wreq              as Wreq
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified Model.Core.References     as R
import           Data.Text                          ( Text           )
import           Text.Read                          ( readMaybe      )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Reader               ( asks, ask      )
import           Control.Monad.Except               ( ExceptT (..)
                                                    , liftIO
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- Acquiring journal sets

getJsets :: T.AppMonad T.JournalSets
getJsets = ask >>= go
    where err  = "Cannot find journal sets."
          go c = case (T.cInputPath c, T.cJsetsYear c) of
                      (Just fp, _      ) -> jsetsFromFile fp
                      (Nothing, Just y ) -> jsetsFromYear y
                      (Nothing, Nothing) -> throwError err

jsetsFromFile :: FilePath -> T.AppMonad T.JournalSets
jsetsFromFile fp = lift . ExceptT $ P.parseJsets <$> Tx.readFile fp

jsetsFromYear :: String -> T.AppMonad T.JournalSets
jsetsFromYear = maybe err go . readMaybe
    where err  = throwError "Invalid year."
          go y = pure . J.yearly26Sets y $ R.issueRefs

getJset :: T.AppMonad T.JournalSet
getJset = do
    let err = "Cannot find requested journal set."
    jsets <- getJsets
    key   <- getJsetNumber
    jset  <- maybe (throwError err) pure . Map.lookup key $ jsets
    pure (key, jset)

getJsetNumber :: T.AppMonad (Int, Int)
getJsetNumber = asks go >>= maybe err pure
    where err  = throwError "Set number must be YEAR-NUMBER."
          go u = case break (== '-') <$> T.cJsetKey u of
                      Just (y,'-':n) -> (,) <$> readMaybe y <*> readMaybe n
                      _              -> Nothing

---------------------------------------------------------------------
-- Routing output

toOutput :: Text -> T.AppMonad Text
toOutput x = asks T.cOutputPath >>= maybe (pure x) go
    where go fp = liftIO (Tx.writeFile fp x) *> pure Tx.empty

---------------------------------------------------------------------
-- Downloading table of contents

writeTocs :: T.AppMonad Text
writeTocs = do
    (k,xs) <- getJset
    fmt    <- asks T.cFormat
    tocs   <- mapM downloadIssueToC xs
    toOutput $ case fmt of
                    T.CSV -> "ToC conversion to csv is not supported."
                    T.MKD -> F.tocsToMkd k tocs
                    T.TXT -> Tx.unlines . map F.tocToTxt $ tocs

downloadIssueToC ::  T.Issue -> T.AppMonad T.TableOfContents
downloadIssueToC x = do
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    toc <- downloadPubMedToc x >>= liftEither . P.parseToC x
    liftIO . Tx.putStrLn $ "OK"
    pure toc

downloadPubMedToc :: T.Issue -> T.AppMonad Text
downloadPubMedToc x = do
    let addr = "https://www.ncbi.nlm.nih.gov/pubmed"
        opts = J.tocQuery x
    resp <- liftIO . Wreq.getWith opts $ addr
    liftEither . J.readResponse $ resp

---------------------------------------------------------------------
-- Displaying journal sets

convert :: T.AppMonad Text
convert = do
    let js = map T.journal R.issueRefs
    jsets <- getJsets
    fmt   <- asks T.cFormat
    toOutput $ case fmt of
                    T.CSV -> F.jsetsToCSV js $ jsets
                    T.MKD -> "Conversion to markdown is not yet implemented."
                    T.TXT -> F.jsetsToTxt $ jsets
