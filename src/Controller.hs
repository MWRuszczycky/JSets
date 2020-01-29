{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    , parseTest
    ) where

import qualified Data.Text.IO      as Tx
import qualified Data.Map.Strict   as Map
import qualified Model.Types       as T
import qualified Model.Core        as C
import qualified Model.Parsers.CSV as P
import qualified Model.Journals    as J
import qualified Model.References  as R
import qualified Viewer            as V

controller :: IO ()
controller = do
    putStrLn "\nWeekly Sets"
    let jsets = J.yearly26Sets 2019 $ R.issueRefs
        table = V.tabulateJSets (map T.journal R.issueRefs) jsets
    Tx.writeFile "dev/output.csv" table
    mapM_ (Tx.putStrLn . V.viewJSet) . Map.toList $ jsets

parseTest :: IO ()
parseTest = do
    testFile <- P.parseCsv <$> Tx.readFile "dev/output.csv"
    case testFile of
         Left err -> putStrLn $ "parse fail: " ++ err
         Right xs -> mapM_ (Tx.putStrLn . C.txt) xs
