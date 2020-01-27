{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    ) where

import qualified Data.Text.IO     as Tx
import qualified Model.Types      as T
import qualified Model.Journals   as J
import qualified Model.References as R
import qualified Viewer           as V

controller :: IO ()
controller = do
    putStrLn "\nWeekly Sets"
    let jsets = J.journalSetsByYear 2019 $ R.issueRefs
        table = V.tabulateSets (map T.journal R.issueRefs) jsets
    Tx.writeFile "testoutput.csv" table
    mapM_ (Tx.putStrLn . V.viewSet) jsets
