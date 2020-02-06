{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( Journal       (..)
    , Frequency     (..)
    , Issue         (..)
    , JournalSet
    , JournalSets
    ) where

import Data.Time       ( Day  )
import Data.Text       ( Text )
import Data.Map.Strict ( Map )

type JournalSet  = ((Int,Int), [Issue])
type JournalSets = Map (Int,Int) [Issue]

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
    , freq   :: Frequency -- Issue frequency
    , resets :: Bool      -- Issue number resets to 1 each year
    } deriving ( Show, Eq )

data Frequency =
      Weekly        -- Every 7 days with no dropped issues (52/year)
    | WeeklyLast    -- Every 7 days dropping the last of the year
    | WeeklyFirst   -- Every 7 days dropping the first of the year
    | Monthly       -- Once every month
      deriving ( Show, Eq )
