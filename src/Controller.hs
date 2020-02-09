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
    case J.lookupIssue "JACS" (140,49) of
         Nothing -> Tx.putStrLn "Invalid issue"
         Just x  -> do result <- J.downloadToC x
                       case result of
                            Left err  -> putStrLn $ "Failed to get ToC: " ++ err
                            Right toc -> Tx.writeFile "dev/toc.html" toc
