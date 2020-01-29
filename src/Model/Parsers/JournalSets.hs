{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJournalSets
    ) where

import qualified Data.Text            as Tx
import qualified Model.Types          as T
import qualified Model.Parsers.CSV    as CSV
import qualified Data.Map.Strict      as Map
import           Text.Read                   ( readMaybe  )
import           Data.Text                   ( Text       )
import           Data.Bifunctor              ( bimap      )
import           Control.Monad               ( foldM      )

parseJournalSets :: Text -> Either String T.JournalSets
parseJournalSets x = CSV.parseCsv x >>= toJournalSets

toJournalSets :: [[Text]] -> Either String T.JournalSets
toJournalSets []     = pure Map.empty
toJournalSets (x:xs) = do
    keys <- toJournalKeys x
    Map.fromList <$> mapM (toJournalSet keys) xs

toJournalKeys :: [Text] -> Either String [Text]
toJournalKeys = undefined

toJournalSet :: [Text] -> [Text] -> Either String T.JournalSet
toJournalSet js xs = undefined
    -- give the journal set a key by number and year

toIssuesInSet :: [Text] -> [Text] -> Either String [T.Issue]
toIssuesInSet _  []     = Left "Empty journal set!"
toIssuesInSet js (_:xs)
    | length js /= length xs = Left "Unmatched issues"
    | otherwise              = foldM go [] . zip js $ xs
    where go iss jt = (<> iss) <$> toIssues jt

toIssues :: (Text, Text) -> Either String [T.Issue]
toIssues (j, t) = do
    vins <- mapM toVolIssNo . Tx.lines $ t
    mapM (lookupIssue j) vins

lookupIssue :: Text -> (Int, Int) -> Either String T.Issue
lookupIssue j (v,n) = undefined

toVolIssNo :: Text -> Either String (Int, Int)
toVolIssNo t = case mapM (readMaybe . Tx.unpack) . Tx.splitOn ":" $ t of
                    Just (v:i:[]) -> pure (v,i)
                    _             -> Left "Cannot parse volume:issue"
                          
