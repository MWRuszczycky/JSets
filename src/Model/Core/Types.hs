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
    , Dict
    , ConfigFile        (..)
    , Config            (..)
    , defaultConfig
    , Command           (..)
    , Format            (..)
    , ToCStyle          (..)
      -- Dates
      -- Journal sets
    , JSet              (..)
    , JSets             (..)
    , References
      -- Journals
    , Journal           (..)
    , Frequency         (..)
      -- Journal issues
    , Issue             (..)
    , Selection         (..)
    , Content           (..)
      -- Table of contents and citations
    , Citation          (..)
    , PageNumber        (..)
    , PMID
      -- Rank matchings
    , MatchCard         (..)
    , MatchResult       (..)
    ) where

import Data.Time            ( Day, toGregorian  )
import Data.Text            ( Text              )
import Data.List            ( foldl', nub, sort )
import Data.Monoid          ( Endo              )
import Control.Monad.Except ( ExceptT           )
import Control.Monad.Reader ( ReaderT, Reader   )
import Control.Monad.Writer ( WriterT           )

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
    , cRefPath    :: FilePath       -- path to the references file
    , cReferences :: [Issue]        -- reference issues
    , cToCStyle   :: ToCStyle       -- table of content style
    , cShowVer    :: Bool           -- show version number flag
    } deriving ( Show )

defaultConfig :: Config
defaultConfig = Config {
      cUser       = Nothing
    , cNick       = Nothing
    , cEmail      = Nothing
    , cOutputPath = Nothing
    , cJSetKey    = Nothing
    , cHelp       = False
    , cRefPath    = ""
    , cReferences = []
    , cToCStyle   = Basic
    , cShowVer    = False
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

data ToCStyle =
      Basic
    | Propose
    | Select
      deriving ( Show, Eq )

-- =============================================================== --
-- Journal sets

-- |A Journal Set (JSet) is a list of all journal issues to be
-- reviewed in a single along with an identifying INT key.
data JSet a = JSet {
      setNo  :: Int
    , issues :: [a]
    } deriving Show

instance HasDate a => HasDate (JSet a) where
    date = maximum . map date . issues

-- The following instance satisifies associativety for mix of JSet
-- whenever associativity of mix is satisifed for a because
--   stir ( stir (xs <> ys) <> zs ) == stir ( (xs <> ys) <> zs              )
--                                  == stir (  xs        <> (ys <> zs)      )
--                                  == stir (  xs        <> stir (ys <> zs) )
instance MayMix a => MayMix (JSet a) where
    mix (JSet n1 s1) (JSet n2 s2)
        | n1 == n2  = pure . JSet n1 . stir $ s1 <> s2
        | otherwise = Nothing

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

-- Journal issues are defined in terms of a progression of types:
-- You start with an *Issue*, make a *Selection* based on the PMIDs
-- of the desired citations in the issue and then add the citations
-- to create a *Content*. Populating the citations may (or may not)
-- be modulated by the PMIDs in the *Selection*.

-- This may not be the best way to structure the data and pipeline.
-- For example, we could just have a single type that has separate
-- fields for the selection and content. This 'progression' approach
-- has caused some headaches, but it works for the most part, and it
-- makes at least some logical sense. For example, a base Issue
-- shouldn't have selected PMIDs at all.

---------------------------------------------------------------------

-- |Basice information about a given issue of a journal.
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

-- |An Issue to which certain citations have been selected for review
-- etc. They are indexed by thier PMIDs. Once the selected citations
-- have been acquired, the Selection becomes a Content.
data Selection = Selection {
      theIssue :: Issue
    , selected :: [PMID]
    } deriving ( Show, Eq )

instance HasDate Selection where
    date = theDate . theIssue

instance HasIssue Selection where
    issue = theIssue

instance MayMix Selection where
    mix (Selection i1 s1) (Selection i2 s2)
        | i1 == i2  = pure . Selection i1 . sort . nub $ s1 <> s2
        | otherwise = Nothing

---------------------------------------------------------------------

-- |A Selection to which the citations associated with the Issue have
-- been added possibly restricted by the selection.
data Content = Content {
      selection :: Selection
    , citations :: [Citation]
    } deriving ( Show, Eq )

instance HasIssue Content where
    issue = theIssue . selection

instance HasDate Content where
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

-- =============================================================== --
-- Rank matching

-- |Basic data type for managing data when performing rank matchings.
-- Each person in the match gets a card that describes their
-- numerical ids, which are required when running the Hungarian
-- algorithm and the their rankings. More than one id is required,
-- because they may be assigned more than one citation.
data MatchCard = MatchCard {
      cardName    :: Text              -- Who the card belongs to
    , cardIDs     :: [Int]             -- Numerical IDs for this card
    , edgeWeights :: [((Int,Int),Int)] -- Weights associated with the card
    } deriving ( Show )          -- First pair is
                                 -- ( citation index, a cardID )
                                 -- Second value is the weight of
                                 -- this weight match-pair

data MatchResult = MatchResult {
      matchTitle :: Text
    , matchings  :: [(Text, [Text])]
    , matchScore :: Int
    } deriving ( Show )
