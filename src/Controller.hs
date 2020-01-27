{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    ) where

import qualified Data.Text.IO     as Tx
import qualified Model.Journals   as J
import qualified Model.References as R
import qualified Viewer           as V

controller :: IO ()
controller = do
    putStrLn "\nWeekly Sets"
    let jsets = J.journalSetsByYear 2019 R.issueRefs
    --let (ws,ms) = J.splitByFreq R.issueRefs
    --    wss     = map (J.issuesInYear 2019) ws
    --    mss     = map (J.issuesInYear 2019) ms
    --    jsets   = J.journalSets wss mss
    mapM_ ( \ j -> Tx.putStrLn (V.viewJournalSet j) >> putStrLn "" ) jsets
