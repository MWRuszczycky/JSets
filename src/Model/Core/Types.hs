{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( --State
      ErrString
    , ErrMonad
    , AppMonad
    , Result            (..)
    , Config            (..)
    , Format            (..)
    , Command           (..)
      -- Journal sets
    , JournalSet        (..)
    , JournalSets       (..)
      -- Journals
    , Journal           (..)
    , Frequency         (..)
      -- Journal issues
    , Issue             (..)
      -- Table of contents and citations
    , JournalSetToC     (..)
    , IssueToC          (..)
    , Citation          (..)
    , PageNumber        (..)
    ) where

import Data.Time            ( Day            )
import Data.Text            ( Text           )
import Data.Map.Strict      ( Map            )
import Control.Monad.Except ( ExceptT        )
import Control.Monad.Reader ( ReaderT        )

-- =============================================================== --
-- State

-- |Error strings
type ErrString = String

-- |IO monad with a fail state
type ErrMonad  = ExceptT ErrString IO

-- |Core application monad
-- |AppMonad a = IO ( Either ErrString ( Reader Config a ) )
type AppMonad  = ReaderT Config ErrMonad

data Result a = Result {
      header :: [Text]
    , result :: a
    }

---------------------------------------------------------------------
-- Program configuration

-- |Data type for output formats
data Format =
      CSV   -- comma separated values
    | TXT   -- text
    | MKD   -- markdown
      deriving ( Show, Eq )

-- |Application configuration
data Config = Config {
      cOutputPath :: Maybe FilePath -- file output path
    , cJsetKey    :: Maybe Int      -- journal set key
    , cFormat     :: Maybe Format   -- output format
    , cHelp       :: Bool           -- user requested help
    } deriving ( Show )

data Command = Command {
      cmdName   :: String
    , cmdAction :: [String] -> AppMonad ()
    , cmdHelp   :: Text
    }

-- =============================================================== --
-- Journal sets

-- |A JournalSet is a list of all journal issues to be reviewed in
-- a single along with an identifying INT key.
data JournalSet  = JSet {
      jsKey    :: Int
    , jsIssues :: [Issue]
    } deriving Show

-- |A JournalSets is a collection of journal sets mapped by key.
newtype JournalSets = JSets (Map Int [Issue])

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

-- |Journal set in terms of tables of contents. The first value is
-- the journal set key.
data JournalSetToC = JSetToC Int [IssueToC]

data IssueToC = IssueToC Issue [Citation] deriving ( Show , Eq )

-- |Information about an article in an issue of a journal
data Citation = Citation {
      title   :: Text
    , authors :: Text
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
