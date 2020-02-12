{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( --State
      ErrString
    , ErrMonad
    , Config        (..)
    , RunMode       (..)
      -- Journal sets
    , JournalSet
    , JournalSets
      -- Journals
    , Journal       (..)
    , Frequency     (..)
      -- Journal issues
    , Issue         (..)
      -- Table of contents and citations
    , TableOfContents
    , Citation      (..)
    , PageNumber    (..)
    ) where

import Data.Time            ( Day            )
import Data.Text            ( Text           )
import Data.Default         ( Default (..)   )
import Data.Map.Strict      ( Map            )
import Control.Monad.Except ( ExceptT        )

-- =============================================================== --
-- State

type ErrString = String

type ErrMonad  = ExceptT ErrString IO

---------------------------------------------------------------------
-- Program setup

data Config = Config {
      cCmds      :: [String]
    , cOutputDir :: Maybe FilePath
    , cJsetsFile :: Maybe FilePath
    , cJsetsYear :: Maybe String
    , cJsetKey   :: Maybe String
    , runMode    :: RunMode
    } deriving ( Show )

instance Default Config where
    def = Config { cCmds      = []
                 , cOutputDir = Nothing
                 , cJsetsFile = Nothing
                 , cJsetsYear = Nothing
                 , cJsetKey   = Nothing
                 , runMode    = NoMode
                 }

data RunMode =
      NoMode
    | ErrMode ErrString
    | HelpMode
    | ToCMode
      deriving ( Show, Eq )

-- =============================================================== --
-- Journal sets

type JournalSet  = ((Int,Int), [Issue])

type JournalSets = Map (Int,Int) [Issue]

-- =============================================================== --
-- Journals

-- |Information about a journal
data Journal = Journal {
      key    :: Text      -- Unique abbreviated title of journal
    , name   :: Text      -- Long name of journal
    , pubmed :: Text      -- Name of journal used by PubMed
    , freq   :: Frequency -- Issue frequency
    , resets :: Bool      -- Issue number resets to 1 each year
    } deriving ( Show, Eq )

-- |Publication frequency of a journal
data Frequency =
      Weekly        -- Every 7 days with no dropped issues (52/year)
    | WeeklyLast    -- Every 7 days dropping the last of the year
    | WeeklyFirst   -- Every 7 days dropping the first of the year
    | Monthly       -- Once every month
      deriving ( Show, Eq )

-- =============================================================== --
-- Journal Issues

-- |Information about a given issue of a journal
data Issue = Issue {
      date    :: Day
    , refNo   :: Int
    , volNo   :: Int
    , issNo   :: Int
    , journal :: Journal
    } deriving ( Show, Eq )

-- =============================================================== --
-- Table of contents and citations

type TableOfContents = [Citation]

-- |Information about an article in an issue of a journal
data Citation = Citation {
      title   :: Text
    , authors :: Text
    , issue   :: Issue
    , pages   :: (PageNumber,PageNumber)
    , doi     :: Text
    } deriving ( Show, Eq )

---------------------------------------------------------------------
-- Page numbers

-- |Page numbers for articles. Page numbers can be prefixed with a
-- character string if necessary.
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
