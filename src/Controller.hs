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
import qualified Viewer                    as V
import           Data.Text                          ( Text )

controller :: IO ()
controller = do
    --let Just jset1 = Map.lookup (2019,1) . J.yearly26Sets 2019 $ R.issueRefs
    --sequence_ . zipWith savePubmed [1..] $ jset1

    -- let Just iss = J.lookupIssue "Science" (2019,6423)
    -- result <- P.parseToC iss <$> Tx.readFile "dev/pubmed12.txt"
    -- case result of
    --      Left err -> putStrLn err
    --      Right toc -> Tx.putStrLn . V.viewToC $ toc

    let Just jset1 = Map.lookup (2019,1) . J.yearly26Sets 2019 $ R.issueRefs
    jsetToMkd ((2019,1), jset1)

-- ================================================================ --
-- Commands

jsetToMkd :: T.JournalSet -> IO ()
jsetToMkd ((_,_),xs) = do
    tocs <- mapM CIO.downloadToC xs
    Tx.writeFile "dev/tocs.mkd" . Tx.unlines . map buildToCMkd $ tocs

buildToCMkd :: Either String T.TableOfContents -> Text
buildToCMkd (Left err)  = Tx.pack err
buildToCMkd (Right toc) = V.tocToMkd toc
