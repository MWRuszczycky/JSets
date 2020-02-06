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

controller :: IO ()
controller = do
    putStrLn "\nWeekly Sets"
    let jsets = J.yearly26Sets 2019 $ R.issueRefs
        table = V.tabulateJSets (map T.journal R.issueRefs) jsets
    Tx.writeFile "dev/output.csv" table
    mapM_ (Tx.putStrLn . V.viewJSet) . Map.toList $ jsets
