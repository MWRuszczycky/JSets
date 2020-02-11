{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( downloadToC
    , downloadToCTxt
    , writeTocsToMkd
    ) where

import qualified Data.Text.IO           as Tx
import qualified Data.Text              as Tx
import qualified Model.Core.Types       as T
import qualified Network.Wreq           as Wreq
import qualified Model.Journals         as J
import qualified Model.Formatting       as F
import qualified Model.Parsers.PubMed   as P
import           Data.Text                      ( Text           )
import           System.IO                      ( hFlush, stdout )
import           Control.Monad.Except           ( ExceptT (..)
                                                , liftIO
                                                , liftEither     )

-- =============================================================== --
-- Commands

---------------------------------------------------------------------
-- Downloading table of contents

writeTocsToMkd :: FilePath -> T.JournalSet -> T.ErrMonad ()
writeTocsToMkd fp (_,xs) = do
    pubmedData <- mapM downloadIssueToC xs
    liftIO . Tx.writeFile fp . Tx.unlines . map F.tocToMkd $ pubmedData
    liftIO . putStrLn $ "Tables of contents written to " <> fp

downloadIssueToC ::  T.Issue -> T.ErrMonad T.TableOfContents
downloadIssueToC x = do
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    toc <- downloadToC x
    liftIO . Tx.putStrLn $ "OK"
    pure toc

downloadToC :: T.Issue -> T.ErrMonad T.TableOfContents
downloadToC x = downloadToCTxt x >>= liftEither . P.parseToC x

downloadToCTxt :: T.Issue -> T.ErrMonad Text
downloadToCTxt x = ExceptT $ J.readResponse <$> Wreq.getWith opts addr
    where addr = "https://www.ncbi.nlm.nih.gov/pubmed"
          opts = J.tocQuery x
