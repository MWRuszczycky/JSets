{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Types
    ( --Classes
      HasDate           (..)
    , HasIssue          (..)
    , MayMix            (..)
    , stirIn
    , stir
    , CanQuery          (..)
      --State
    , ErrString
    , ErrMonad
    , AppMonad
    , ViewMonad
      -- Program configuration
    , Configurator
    , ConfigStep        (..)
    , Config            (..)
    , defaultConfig
    , Command           (..)
    , Format            (..)
    , Color             (..)
      -- Journal sets
    , JSet              (..)
    , JSets             (..)
    , References
      -- Meetings
    , Presenter
    , Meeting           (..)
      -- PubMed
    , ESearchTerm
    , Query
    , QueryTerm         (..)
      -- Journals
    , Journal           (..)
    , Frequency         (..)
      -- Journal issues
    , Issue             (..)
    , ToC               (..)
      -- Table of contents and citations
    , PMID
    , DOI
    , Selection         (..)
    , Citation          (..)
    , Citations
    , PageNo            (..)
    , PageRange         (..)
      -- Rank matchings
    , MatchResult       (..)
    ) where

import qualified Data.Text             as Tx
import           Control.Monad.Except        ( ExceptT           )
import           Control.Monad.Reader        ( ReaderT, Reader   )
import           Control.Monad.Writer        ( WriterT           )
import           Data.List                   ( foldl', nub       )
import           Data.Map.Strict             ( Map               )
import           Data.Monoid                 ( Endo              )
import           Data.Text                   ( Text              )
import           Data.Time                   ( Day, toGregorian
                                             , fromGregorian     )

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

---------------------------------------------------------------------
-- Things that can be queried at PubMed

class CanQuery a where
    query :: a -> Query

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

-- |A Configurator is used to update the configuration (Config) based
-- on a command line option or a configuration file option.
type Configurator = Config -> ErrMonad Config

-- |A ConfigStep is a step during the configuration process where the
-- configuration (Config) is updated in some way. It either wraps a
-- Configurator that needs to be applied or a warning that was raised
-- during the reading of configuration options from the command line
-- or file. Init steps should be applied early in the configuration
-- process as later configuration steps may depend on these results
-- (e.g., setting the configuration file path). Gen steps take place
-- later and should not affect the outcome of subseqeunt steps.
data ConfigStep =
      ConfigInit Configurator  -- Initial step, may affect later steps
    | ConfigGen  Configurator  -- General step, should not affect later steps
    | ConfigWarn Text          -- Configuration warning

-- |Application configuration
data Config = Config {
      -- General configuraton
      cArguments     :: [String]       -- command line arguments
    , cDate          :: Day            -- date when application started
    , cStdOutIsTerm  :: Bool           -- stdout is the terminal
    , cUseANSI       :: Bool           -- use ANSI control sequences
    , cUser          :: Maybe Text     -- user's name
    , cEmail         :: Maybe Text     -- user's email
    , cFormat        :: Format         -- explicit output format
    , cTerse         :: Bool           -- do not produce messages
      -- Special command flags
    , cHelp          :: Bool           -- user requested help
    , cShowVer       :: Bool           -- show version number flag
      -- File paths
    , cOutputPath    :: Maybe FilePath -- file output path
    , cConfigPath    :: Maybe FilePath -- path to the configuration file
    , cErrorLog      :: FilePath       -- where to send detailed error info
      -- Journal sets
    , cJSetKey       :: Maybe Int      -- journal set key
    , cReferences    :: [Issue]        -- reference issues
    , cYearlyByDate  :: Bool           -- group issues by date with <year> cmd
      -- PubMed
    , cQuery         :: Query          -- arguments for PubMed queries
    , cMaxResults    :: Int            -- maximum results from an ESearch query
    , cESumChunkSize :: Int            -- esummary chunk size/PubMed download
    , cDelay         :: Integer        -- delay (sec) between PubMed requests
    , cOnlyPMIDs     :: Bool           -- only return PMIDs from PubMed query
      -- Matching
    , cMatchDetails  :: Bool           -- show detailed match output
    , cMatchTemplate :: Bool           -- generate a match template with <match>
      -- Literature Review
    , cPresenters    :: [Presenter]    -- who will present at Lit Reviews
    , cPresenterOne  :: Maybe Presenter -- who will present Lit Review first
    , cMeetingSize   :: Int            -- Number of presenters per meeting
    , cStartDay      :: Maybe Day      -- First meeting day
    , cSkipDays      :: [Day]          -- days to skip when assigning meetings
    , cMeetPattern   :: [Bool]         -- meeting pattern
    , cMeetCount     :: Int            -- number of meetings to schedule
    } deriving ( Show )

defaultConfig :: Config
defaultConfig = Config {
      -- General defaults
      cArguments     = []
    , cDate          = fromGregorian 2020 1 1
    , cStdOutIsTerm  = True
    , cUseANSI       = False
    , cUser          = Nothing
    , cEmail         = Nothing
    , cFormat        = TXT
    , cTerse         = False
      -- Default command flags
    , cHelp          = False
    , cShowVer       = False
      -- Default file paths
    , cOutputPath    = Nothing
    , cConfigPath    = Nothing
    , cErrorLog      = "jsets-errors.log"
      -- Default journal set parameters
    , cJSetKey       = Nothing
    , cReferences    = []
    , cYearlyByDate  = False
      -- Default PubMed parameters
    , cQuery         = []
    , cMaxResults    = 200
    , cESumChunkSize = 300
    , cDelay         = 1
    , cOnlyPMIDs     = False
      -- Default matching parameters
    , cMatchDetails  = False
    , cMatchTemplate = False
      -- Literature Review defaults
    , cPresenters    = []
    , cPresenterOne  = Nothing
    , cMeetingSize   = 3
    , cStartDay      = Nothing
    , cSkipDays      = []
    , cMeetPattern   = [True, False]
    , cMeetCount     = 10
    }

-- |Describes a JSets command that can be run (e.g., <toc> or <read>)
data Command = Command {
      cmdName   :: String                   -- How the command is envoked
    , cmdAction :: [String] -> AppMonad ()  -- What it does with its arguments
    , cmdHelp   :: (Text, Text)             -- Short and long documentation
    }

-- |Output formats.
data Format =
      TXT
    | HTML
    | MKD
    | CSV
    | JSON
      deriving ( Show, Eq )

-- |Terminal colors
data Color =
      Red
    | Green
    | Yellow
      deriving ( Show, Eq )

-- =============================================================== --
-- Journal sets

-- |A Journal Set (JSet) is a list of all journal issues to be
-- reviewed in a single litarature review session along with an
-- identifying integer key.
data JSet a = JSet {
      setNo     :: Int
    , issues    :: [a]
    , selection :: [Selection]
    } deriving Show

instance HasDate a => HasDate (JSet a) where
    date = maximum . map date . issues

instance MayMix a => MayMix (JSet a) where
    mix (JSet n1 i1 s1) (JSet n2 i2 s2)
        | n1 == n2  = pure $ JSet n1 i3 s3
        | otherwise = Nothing
        where i3 = stir $ i1 <> i2
              s3 = stir $ s1 <> s2

-- |A Collection of journal sets
newtype JSets a = JSets [JSet a]

-- |A list of reference issues
type References = [Issue]

-- ================================================================== 
-- Meetings

-- |Name of someone who will present at a meeting
type Presenter = Text

data Meeting a = Meeting {
      presenters  :: [Presenter] -- who will present
    , toReview    :: a           -- whatever will be presented
    , meetingDate :: Day         -- when the meeting will take place
    } deriving ( Show )

instance HasDate (Meeting a) where
    date = meetingDate

-- =============================================================== --
-- PubMed

-- |The text string which is built from a Query and is appended to
-- the ESearch url to submit a PubMed ESearch request.
type ESearchTerm = Text

-- |Representation of a complete PubMed ESearch request.
type Query = [QueryTerm]

-- |Should correspond to a single field in a PubMed ESearch request.
data QueryTerm =
      AuthorQry  Text
    | TitleQry   Text
    | PageQry    PageNo
    | DOIQry     Text
    | JournalQry Text
    | WildQry    Text
    | PMIDQry    Text
    | YearQry    Int
    | NumberQry  Int
    | VolumeQry  Int
      deriving ( Eq, Show )

instance CanQuery a => CanQuery [a] where
    query = concatMap query

instance CanQuery QueryTerm where
    query = (:[])

-- =============================================================== --
-- Journals

-- |Information about a journal
data Journal = Journal {
      abbr     :: Text      -- Abbreviated title of journal
    , name     :: Text      -- Long name of journal
    , pubmed   :: Text      -- Name of journal used by PubMed
    , freq     :: Frequency -- Issue frequency
    , resets   :: Bool      -- Issue number resets to 1 each year
    , mincount :: Int       -- Fewer citations suggests some are missing
    , followed :: Bool      -- Include journal in <year> calculation
    } deriving ( Show, Eq )

-- |Publication frequency of a journal
-- If the frequency is EveryNWeeks and N is less than 1, then the
-- reference issues is assumed to be the only issue published.
data Frequency =
      EveryNWeeks Int -- Every 7 * N days with no dropped issues
    | WeeklyLast      -- Every 7 days dropping the last of the year
    | WeeklyFirst     -- Every 7 days dropping the first of the year
    | Monthly         -- Once every month towards beginning of the month
    | MidMonthly      -- Once every motnh towards end of the month
    | EndMonthly      -- Once by the end of every month
    | SemiMonthly     -- Twice every month
    | UnknownFreq     -- The frequency is not known
      deriving ( Show, Eq )

-- =============================================================== --
-- Journal Issues

---------------------------------------------------------------------

-- |Base description of a published issue of a journal.
data Issue = Issue {
      theDate    :: Day
    , theVolNo   :: Int
    , theIssNo   :: Int
    , theJournal :: Journal
    } deriving ( Eq )

instance Show Issue where
    show x = (Tx.unpack . abbr . theJournal) x <> " "
             <> (show . theVolNo) x <> ":"
             <> (show . theIssNo) x

instance HasDate Issue where
    date = theDate

instance HasIssue Issue where
    issue = id

instance MayMix Issue where
    mix i1 i2
        | i1 == i2  = Just i1
        | otherwise = Nothing

instance CanQuery Issue where
    query iss = [ JournalQry . pubmed . journal $ iss
                , YearQry . year $ iss
                , NumberQry . issNo $ iss
                ]

---------------------------------------------------------------------

-- |Essentially the table of contents associated with an Issue. It
-- provides all the PubMed IDs (or as many as can be found) of the
-- citations in the issue as well a possible URL where the ToC can be
-- found at the publisher's website if necessary.
data ToC = ToC {
      theIssue :: Issue  -- The issue the contents reflect.
    , tocURL   :: Text   -- URL to the online ToC at the publisher's website.
    , contents :: [PMID] -- PubMed IDs for all articles in the issue.
    } deriving ( Show )

instance HasIssue ToC where
    issue = theIssue

instance HasDate ToC where
    date = theDate . theIssue

instance MayMix ToC where
    mix c1 c2
        | issue c1 == issue c2 = Just c1 { tocURL = u, contents = xs }
        | otherwise            = Nothing
        where xs = nub $ contents c1 <> contents c2
              u  | Tx.null (tocURL c1) = tocURL c2
                 | otherwise           = tocURL c1

-- =============================================================== --
-- Citations

-- |A PubMed ID
type PMID = Text

-- |A digital object identifier
type DOI  = Text

-- |Structured data type representing user citation selections.
-- Normally the selection should be by PMID; however, in some cases
-- the user may specify a DOI, or some web identifier (e.g., website,
-- article title, etc.). The PMID & DOI can be bound to known issue.
data Selection =
      ByBndPMID Issue PMID -- PubMed ID that should be bound to an issue
    | ByPMID          PMID -- PubMed ID where the associated issue is not known
    | ByBndDOI  Issue Text -- DOI that should be bound to an issue
    | ByDOI           Text -- DOI where the associated issue is not known
    | ByWeb           Text -- Some text identifier to help locate the selection
      deriving ( Eq )

instance Show Selection where
    show (ByBndPMID i x) = Tx.unpack x <> " (pmid bound to " <> show i <> ")"
    show (ByPMID      x) = Tx.unpack x <> " (unbound pmid)"
    show (ByBndDOI  i x) = Tx.unpack x <> " (doi bound to " <> show i <> ")"
    show (ByDOI       x) = Tx.unpack x <> " (unbound doi)"
    show (ByWeb       x) = Tx.unpack x <> " (web identifier)"

instance MayMix Selection where
    mix (ByBndPMID x1 x2) (ByBndPMID y1 y2)
        | x1 == y1 && x2 == y2 = Just $ ByBndPMID x1 x2
        | otherwise            = Nothing
    mix (ByPMID x) (ByPMID y)
        | x == y               = Just $ ByPMID x
        | otherwise            = Nothing
    mix (ByBndDOI x1 x2) (ByBndDOI y1 y2)
        | x1 == y1 && x2 == y2 = Just $ ByBndDOI x1 x2
        | otherwise            = Nothing
    mix (ByDOI x) (ByDOI y)
        | x == y               = Just $ ByDOI x
        | otherwise            = Nothing
    mix (ByWeb x) (ByWeb y)
        | x == y               = Just $ ByWeb x
        | otherwise            = Nothing
    mix _ _ = Nothing

instance CanQuery Selection where
      query (ByBndPMID _ p) = [PMIDQry p]
      query (ByPMID      p) = [PMIDQry p]
      query (ByBndDOI  _ d) = [DOIQry  d]
      query (ByDOI       d) = [DOIQry  d]
      query (ByWeb       l) = [WildQry l]

-- |Information about an article in an issue of a journal
data Citation = Citation {
      title    :: Text      -- title of the article
    , authors  :: [Text]    -- authors of the article
    , pubIssue :: Issue     -- journal issue the article was published
    , pages    :: PageRange -- page numbers of the article
    , doi      :: DOI       -- digital object identifier
    , pmid     :: PMID      -- PubMed ID (or another unique identifier tag)
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
    show Online          = "online"
    show (InPrint p1 p2) | p1 == p2  = show p1
                         | otherwise = show p1 <> "-" <> show p2

instance Ord PageRange where
    compare Online         Online         = EQ
    compare Online         _              = GT
    compare _              Online         = LT
    compare (InPrint p1 _) (InPrint p2 _) = compare p1 p2

-- =============================================================== --
-- Rank matching

-- |Data type used to manage the results from the <match> command.
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
