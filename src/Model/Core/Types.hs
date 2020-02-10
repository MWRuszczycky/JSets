{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( PageNumber    (..)
    , Citation      (..)
    , Journal       (..)
    , Frequency     (..)
    , Issue         (..)
    , JournalSet
    , JournalSets
    , TableOfContents
    ) where

import Data.Time       ( Day  )
import Data.Text       ( Text )
import Data.Map.Strict ( Map )

type JournalSet      = ((Int,Int), [Issue])
type JournalSets     = Map (Int,Int) [Issue]
type TableOfContents = [Citation]

data PageNumber = PageNumber String Int deriving ( Eq )

instance Show PageNumber where
    show (PageNumber p d) = p <> show d

instance Ord PageNumber where
    compare (PageNumber p1 d1) (PageNumber p2 d2)
        | null p1 && null p2 = compare d1 d2
        | null p1            = LT
        | null p2            = GT
        | p1 == p2           = compare d1 d2
        | otherwise          = compare p1 p2

data Issue = Issue {
      date    :: Day
    , refNo   :: Int
    , volNo   :: Int
    , issNo   :: Int
    , journal :: Journal
    } deriving ( Show, Eq )

data Journal = Journal {
      key    :: Text      -- Unique abbreviated title of journal
    , name   :: Text      -- Long name of journal
    , pubmed :: Text      -- Name of journal used by PubMed
    , freq   :: Frequency -- Issue frequency
    , resets :: Bool      -- Issue number resets to 1 each year
    } deriving ( Show, Eq )

data Frequency =
      Weekly        -- Every 7 days with no dropped issues (52/year)
    | WeeklyLast    -- Every 7 days dropping the last of the year
    | WeeklyFirst   -- Every 7 days dropping the first of the year
    | Monthly       -- Once every month
      deriving ( Show, Eq )

data Citation = Citation {
      title   :: Text
    , authors :: Text
    , issue   :: Issue
    , pages   :: (PageNumber,PageNumber)
    , doi     :: Text
    } deriving ( Show, Eq )
