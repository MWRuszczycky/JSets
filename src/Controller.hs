{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Model.Core.References     as R
import qualified Model.Journals            as J
import qualified CoreIO                    as CIO
import qualified Model.Formatting          as F
import           Control.Monad.Except               ( liftIO )
import           System.IO                          ( hFlush
                                                    , stdout )

controller :: T.ErrMonad ()
controller = do
    let Just jset1 = Map.lookup (2019,1) . J.yearly26Sets 2019 $ R.issueRefs
    writeToC "dev/tocs.mkd" ((2019,1), jset1)

-- ================================================================ --
-- Commands

writeToC :: FilePath -> T.JournalSet -> T.ErrMonad ()
writeToC fp (_,xs) = do
    pubmedData <- mapM downloadIssueToC xs
    liftIO . Tx.writeFile fp . Tx.unlines . map F.tocToMkd $ pubmedData
    liftIO . putStrLn $ "Tables of contents written to " <> fp

downloadIssueToC ::  T.Issue -> T.ErrMonad T.TableOfContents
downloadIssueToC x = do
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    toc <- CIO.downloadToC x
    liftIO . Tx.putStrLn $ "OK"
    pure toc
