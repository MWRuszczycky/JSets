{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( --State
      ErrString
    , ErrMonad
    , AppMonad
    , Dict
    , ConfigFile        (..)
    , Config            (..)
    , Command           (..)
    , Format            (..)
    , ToCStyle          (..)
      -- Dates
    , HasDate           (..)
      -- Journal sets
    , JournalSet        (..)
    , Collection
    , References
      -- Journals
    , Journal           (..)
    , Frequency         (..)
      -- Journal issues
    , Issue             (..)
    , Selection         (..)
    , IssueContent      (..)
    , HasIssue          (..)
      -- Table of contents and citations
    , Citation          (..)
    , PageNumber        (..)
    , PMID
    ) where

import Data.Time            ( Day, toGregorian )
import Data.Text            ( Text             )
import Data.Map.Strict      ( Map              )
import Control.Monad.Except ( ExceptT          )
import Control.Monad.Reader ( ReaderT          )

-- =============================================================== --
-- State

-- |Error strings
type ErrString = String

-- |IO monad with a fail state
type ErrMonad  = ExceptT ErrString IO

-- |Core application monad
-- |AppMonad a = IO ( Either ErrString ( Reader Config a ) )
type AppMonad  = ReaderT Config ErrMonad

---------------------------------------------------------------------
-- Program configuration

type Dict = [(Text, Text)]

-- |Structured data from a parsed configuration file. The first Dict
-- is the header information. The second Dict is the references.
data ConfigFile = ConfigFile Dict [Dict] deriving ( Show )

-- |Application configuration
data Config = Config {
      cUser       :: Maybe Text     -- user's full formal name
    , cNick       :: Maybe Text     -- user's short/nickname
    , cEmail      :: Maybe Text     -- user's email
    , cOutputPath :: Maybe FilePath -- file output path
    , cJsetKey    :: Maybe Int      -- journal set key
    , cHelp       :: Bool           -- user requested help
    , cRefPath    :: FilePath       -- path to the references file
    , cReferences :: [Issue]        -- reference issues
    , cToCStyle   :: ToCStyle       -- table of content style
    , cShowVer    :: Bool           -- show version number flag
    } deriving ( Show )

data Command = Command {
      cmdName   :: String
    , cmdAction :: [String] -> AppMonad ()
    , cmdHelp   :: (Text, Text)
    }

data Format =
      TXT
    | HTML
    | MKD
    | CSV
      deriving ( Show, Eq )

data ToCStyle =
      Propose
    | Select
    | Rank
      deriving ( Show, Eq )

-- =============================================================== --
-- Dates

class HasDate a where
      date  :: a -> Day
      year  :: a -> Int
      month :: a -> Int
      day   :: a -> Int
      year  x = let (y,_,_) = toGregorian . date $ x in fromIntegral y
      month x = let (_,m,_) = toGregorian . date $ x in m
      day   x = let (_,_,d) = toGregorian . date $ x in d

instance HasDate Day where
    date = id

-- =============================================================== --
-- Journal sets

-- |A JournalSet is a list of all journal issues to be reviewed in
-- a single along with an identifying INT key.
data JournalSet  a = JSet {
      setNo  :: Int
    , issues :: [a]
    } deriving Show

instance HasDate a => HasDate (JournalSet a) where
    date = maximum . map date . issues

-- |A Collection basic journal sets mapped by set number.
type Collection a = Map Int [a]

-- |A list of reference issues
type References = [Issue]

-- =============================================================== --
-- Journals

-- |Information about a journal
data Journal = Journal {
      abbr   :: Text      -- Abbreviated title of journal
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

class HasDate a => HasIssue a where
    issue   :: a -> Issue
    journal :: a -> Journal
    volNo   :: a -> Int
    issNo   :: a -> Int
    journal = theJournal . issue
    volNo   = theVolNo   . issue
    issNo   = theIssNo   . issue

---------------------------------------------------------------------

-- |Information about a given issue of a journal
data Issue = Issue {
      theDate    :: Day
    , theVolNo   :: Int
    , theIssNo   :: Int
    , theJournal :: Journal
    } deriving ( Show, Eq )

instance HasDate Issue where
    date = theDate

instance HasIssue Issue where
    issue = id

---------------------------------------------------------------------

data Selection = Selection {
      theIssue :: Issue
    , selected :: [PMID]
    } deriving ( Show, Eq )

instance HasDate Selection where
    date = theDate . theIssue

instance HasIssue Selection where
    issue = theIssue

---------------------------------------------------------------------

data IssueContent = IssueContent {
      selection :: Selection
    , citations :: [Citation]
    } deriving ( Show, Eq )

instance HasIssue IssueContent where
    issue = theIssue . selection

instance HasDate IssueContent where
    date = theDate . theIssue . selection

-- =============================================================== --
-- Citations and selections

-- |Information about an article in an issue of a journal
data Citation = Citation {
      title   :: Text
    , authors :: [Text]
    , pages   :: (PageNumber, PageNumber)
    , doi     :: Text
    , pmid    :: PMID
    } deriving ( Show, Eq )

type PMID = Text

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
