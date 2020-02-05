{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.JournalSets
    ( parseJournalSets
    ) where

import qualified Data.Text            as Tx
import qualified Model.Types          as T
import qualified Model.Parsers.CSV    as CSV
import qualified Model.References     as R
import qualified Model.Journals       as J
import qualified Data.Map.Strict      as Map
import           Text.Read                   ( readMaybe  )
import           Data.Text                   ( Text       )
import           Data.Bifunctor              ( bimap      )
import           Control.Monad               ( foldM      )

parseJournalSets :: Text -> Either String T.JournalSets
parseJournalSets x = CSV.parseCSV x >>= toJournalSets

toJournalSets :: [[Text]] -> Either String T.JournalSets
toJournalSets []     = pure Map.empty
toJournalSets (x:xs) = do
    jKeys <- toJournalKeys x
    jSets <- traverse (toJournalSet jKeys) xs
    if duplicateKeys jSets
       then Left "There duplicated journal set keys."
       else pure . Map.fromList $ jSets

duplicateKeys :: [T.JournalSet] -> Bool
duplicateKeys = go . fst . unzip
    where go []     = False
          go (x:xs) = elem x xs || go xs

toJournalKeys :: [Text] -> Either String [Text]
toJournalKeys []     = Left "Nothing to parse."
toJournalKeys (_:[]) = Left "Nothing to parse."
toJournalKeys (_:ks) = traverse go ks
    where errMsg x = "Journal key unavailable: " ++ Tx.unpack x
          go x | R.isAvailable x = pure x
               | otherwise       = Left . errMsg $ x

toJournalSet :: [Text] -> [Text] -> Either String T.JournalSet
toJournalSet _  []     = Left "Missing key for journal set."
toJournalSet js (x:xs) = (,) <$> parseKey x <*> toIssuesInSet js xs

parseKey :: Text -> Either String Text
parseKey x
    | Tx.null key = Left "Missing key for journal set."
    | otherwise   = pure key
    where key = case Tx.lines x of
                     (y:_) -> Tx.strip y
                     _     -> Tx.empty

toIssuesInSet :: [Text] -> [Text] -> Either String [T.Issue]
toIssuesInSet js xs = fmap concat . traverse toIssues . zip js $ ys
    where ys = xs ++ replicate (length js - length xs) Tx.empty

toIssues :: (Text, Text) -> Either String [T.Issue]
toIssues (j, t) = mapM toVolIssNo xs >>= mapM (go . J.lookupIssue j)
    where xs          = Tx.lines t
          go (Just x) = pure x
          go Nothing  = Left . Tx.unpack . Tx.unwords $ [ "Missing Issue(s):"
                                                        , j, Tx.unwords xs
                                                        ]

toVolIssNo :: Text -> Either String (Int, Int)
toVolIssNo t = case traverse (readMaybe . Tx.unpack) . Tx.splitOn ":" $ t of
                    Just (v:i:[]) -> pure (v,i)
                    _             -> Left "Cannot parse volume:issue"
