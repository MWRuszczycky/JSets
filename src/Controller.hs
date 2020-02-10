{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Model.Core.References     as R
import qualified Model.Journals            as J
import qualified Viewer                    as V

import qualified Model.Parsers.PubMed      as P

controller :: IO ()
controller = do
    let Just iss = J.lookupIssue "JACS" (140,49)
    testFile <- Tx.readFile "dev/toc.html"
    case P.parseToC iss testFile of
         Left err  -> putStrLn $ "Fail: " <> err
         Right toc -> Tx.putStrLn . V.viewToC $ toc
