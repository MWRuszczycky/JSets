{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( --Classes
      HasDate           (..)
    , HasIssue          (..)
    , MayMix            (..)
    , stirIn
    , stir
      --State
    , ErrString
    , ErrMonad
    , AppMonad
    , ViewMonad
    , Version           (..)
      -- Program configuration
    , Dict
    , ConfigFile        (..)
    , Config            (..)
    , defaultConfig
    , Command           (..)
    , Format            (..)
      -- Journal sets
    , JSet              (..)
    , JSets             (..)
    , References
      -- Journals
    , Journal           (..)
    , Frequency         (..)
      -- Journal issues
    , Issue             (..)
    , Content           (..)
      -- Table of contents and citations
    , PMID
    , Citation          (..)
    , Citations
    , PageNo            (..)
    , PageRange         (..)
      -- Rank matchings
    , MatchResult       (..)
    ) where

import qualified Data.Text             as Tx
import           Data.Time                   ( Day, toGregorian  )
import           Data.Text                   ( Text              )
import           Data.List                   ( foldl', nub       )
import           Data.Monoid                 ( Endo              )
import           Control.Monad.Except        ( ExceptT           )
import           Control.Monad.Reader        ( ReaderT, Reader   )
import           Control.Monad.Writer        ( WriterT           )
import           Data.Map.Strict             ( Map               )

-- =============================================================== --
-- Classes

---------------------------------------------------------------------
-- Things that have dates

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

---------------------------------------------------------------------
-- Things that have an issue and associated journal
class HasDate a => HasIssue a where
    issue   :: a -> Issue
    journal :: a -> Journal
    volNo   :: a -> Int
    issNo   :: a -> Int
    journal = theJournal . issue
    volNo   = theVolNo   . issue
    issNo   = theIssNo   . issue

---------------------------------------------------------------------
-- Things that may be combinable like a semigroup, but not always.
-- In other words certain *values* can combine like a semigroup.
-- Miscibility should be transitive:
--      a mixes with b and b mixes with c implies a mixes with c
-- Miscibility should be reflexive:
--      a mixes with itself
-- Miscibility should be symmetric
--      a mixes with b implies b mixes with a
-- Mixing should be associative:
--      if a, b, & c are miscible then
--          (a `mix` b) `mix` c == a `mix` (b `mix` c)
class MayMix a where
    -- |Minimal definition defining when two values can mix and how
    -- they do so. If they cannot mix, then mix evaluates to Nothing.
    mix :: a -> a -> Maybe a

    -- |Determine whether two MayMix values are miscible.
    miscible :: a -> a -> Bool
    miscible x y = maybe False (const True) . mix x $ y

stirIn :: MayMix a => [a] -> a -> [a]
-- ^Add a value to a list combining it with the first value it is
-- miscible with. Otherwise, add it to the end of the list. The
-- stirred value mixes right-to-left with the first miscible element
-- of the list, i.e., x_miscible_in_list `mix` x_stirred_in
stirIn []     x0 = [x0]
stirIn (x:xs) x0 = maybe ( x : stirIn xs x0 ) (: xs) . mix x $ x0

stir :: MayMix a => [a] -> [a]
-- ^Combine all miscible values in a list. Stir maintains the
-- original order of first-unmiscible elemenents. For example,
-- if xs = [ x1, x2, y1, z1, x3, z2, y2 ]
-- then stir xs == [ ((x1 `mix` x2) `mix` x3), (y1 `mix` y2), (z1 `mix` z2) ]
-- where mix is associative. Therefore, we get the following,
--     stir ( stir xs )       == stir xs
--     stir ( stir xs <> ys ) == stir ( xs <> ys ) == stir ( xs <> stir ys )
-- because mix is associative.
stir = foldl' stirIn []

-- =============================================================== --
-- State

-- |Error strings
type ErrString = String

-- |IO monad with a fail state
type ErrMonad  = ExceptT ErrString IO

-- |Core application monad
-- |AppMonad a = IO ( Either ErrString ( Reader Config a ) )
type AppMonad  = ReaderT Config ErrMonad

-- |Non-IO monad for use with generating formatted output
type ViewMonad = WriterT (Endo [Text]) (Reader Config)

-- |JSets version. This is handled in View.Help
data Version =
      RelVersion Text
    | DevVersion Text
      deriving (Show, Eq)

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
    , cJSetKey    :: Maybe Int      -- journal set key
    , cHelp       :: Bool           -- user requested help
    , cRefPath    :: Maybe FilePath -- path to the references file
    , cReferences :: [Issue]        -- reference issues
    , cSortJSets  :: Bool           -- sort issues by Journal in output
    , cInstrToc   :: Bool           -- show instructions it htms ToC
    , cShowVer    :: Bool           -- show version number flag
    , cVerbose    :: Bool           -- use verbose output
    , cDelay      :: Integer        -- Delay in seconds between PubMed requests
    } deriving ( Show )

defaultConfig :: Config
defaultConfig = Config {
      cUser       = Nothing
    , cNick       = Nothing
    , cEmail      = Nothing
    , cOutputPath = Nothing
    , cJSetKey    = Nothing
    , cHelp       = False
    , cRefPath    = Nothing
    , cReferences = []
    , cSortJSets  = True
    , cInstrToc   = False
    , cShowVer    = False
    , cVerbose    = False
    , cDelay      = 1
    }

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

-- =============================================================== --
-- Journal sets

-- |A Journal Set (JSet) is a list of all journal issues to be
-- reviewed in a single along with an identifying INT key.
data JSet a = JSet {
      setNo     :: Int
    , issues    :: [a]
    , selection :: [PMID]
    } deriving Show

instance HasDate a => HasDate (JSet a) where
    date = maximum . map date . issues

instance MayMix a => MayMix (JSet a) where
    mix (JSet n1 i1 s1) (JSet n2 i2 s2)
        | n1 == n2  = pure $ JSet n1 i3 s3
        | otherwise = Nothing
        where i3 = stir $ i1 <> i2
              s3 = nub  $ s1 <> s2

-- |A Collection of journal sets
newtype JSets a = JSets [JSet a]

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

-- An Issue is the base description of a published issue of a journal.
-- A Content is essentially the table of contents associated witha an
-- Issue. It provides all the PubMed IDs of citations in the issue as
-- well a possible URL where the ToC can be found at the publisher's
-- website if necessary.

---------------------------------------------------------------------

-- |Basic information about a given issue of a journal.
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

instance MayMix Issue where
    mix i1 i2
        | i1 == i2  = Just i1
        | otherwise = Nothing

---------------------------------------------------------------------

-- |Table of contents for an issue of a journal
data Content = Content {
      theIssue :: Issue  -- The issue the contents reflect.
    , tocURL   :: Text   -- URL to the online ToC at the publisher's website.
    , contents :: [PMID] -- PubMed IDs for all articles in the issue.
    } deriving ( Show )

instance HasIssue Content where
    issue = theIssue

instance HasDate Content where
    date = theDate . theIssue

instance MayMix Content where
    mix c1 c2
        | issue c1 == issue c2 = Just c1 { tocURL = u, contents = xs }
        | otherwise            = Nothing
        where xs = nub $ contents c1 <> contents c2
              u  | Tx.null (tocURL c1) = tocURL c2
                 | otherwise           = tocURL c1

-- =============================================================== --
-- Citations

type PMID = Text

-- |Information about an article in an issue of a journal
data Citation = Citation {
      title    :: Text
    , authors  :: [Text]
    , pubIssue :: Issue
    , pages    :: PageRange
    , doi      :: Text
    , pmid     :: PMID
    } deriving ( Show, Eq )

instance HasDate Citation where
    date = date . pubIssue

instance HasIssue Citation where
    issue = pubIssue

type Citations = Map PMID Citation

---------------------------------------------------------------------
-- Page numbers

-- |Page numbers for articles. Page numbers can be prefixed with a
-- character string if necessary.
data PageNo = PageNo String Int deriving ( Eq )

instance Show PageNo where
    show (PageNo p d) = p <> show d

instance Ord PageNo where
    compare (PageNo p1 d1) (PageNo p2 d2)
        | null p1 && null p2 = compare d1 d2
        | null p1            = LT
        | null p2            = GT
        | p1 == p2           = compare d1 d2
        | otherwise          = compare p1 p2

data PageRange =
     InPrint PageNo PageNo
   | Online
     deriving ( Eq )

instance Show PageRange where
    show (InPrint p1 p2) = show p1 <> "-" <> show p2
    show Online          = "online"

instance Ord PageRange where
    compare Online         Online         = EQ
    compare Online         _              = GT
    compare _              Online         = LT
    compare (InPrint p1 _) (InPrint p2 _) = compare p1 p2

-- =============================================================== --
-- Rank matching

data MatchResult = MatchResult {
      -- Identifier title for the match.
      matchTitle  :: Text
      -- Paper indices being matched. Negative values are phantoms.
    , matchPapers :: [Int]
      -- ID map (Person, List of indices for this person)
    , matchIDs    :: [(Text, [Int])]
      -- Scores used for assignemnt ( (paper index, match ID), score )
    , matchScores :: [((Int,Int),Int)]
      -- Result of the matching ( total score, [(paper index, match ID)] )
    , matchResult :: Either ErrString ( Int, [(Int,Int)] )
    } deriving ( Show )
