{-# LANGUAGE OverloadedStrings #-}

module Model.Types
    ( Journal   (..)
    , Frequency (..)
    , Issue     (..)
    ) where

import Data.Time ( Day  )
import Data.Text ( Text )

data Issue = Issue {
      date    :: Day
    , refNo   :: Int
    , volNo   :: Int
    , issNo   :: Int
    , journal :: Journal
    } deriving ( Show, Eq )

data Journal = Journal {
      name   :: Text      -- Long name of journal
    , abbr   :: Text      -- Abbreviated name
    , freq   :: Frequency -- Issue frequency
    , resets :: Bool      -- Issue number resets to 1 each year
    } deriving ( Show, Eq )

data Frequency =
      Weekly        -- Every 7 days with no dropped issues (52/year)
    | WeeklyLast    -- Every 7 days dropping the last of the year
    | WeeklyFirst   -- Every 7 days dropping the first of the year
    | Monthly       -- Once every month
      deriving ( Show, Eq )
