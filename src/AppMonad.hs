{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE MultiWayIf        #-}

module AppMonad
    ( -- Data structure construction & aquisition
      readJSets
    , getJSet
    , getJSets
      -- Working with configured reference issues
    , references
    , getIssue
      -- Running rank matchings
    , runMatch
      -- Scheduling Literature Review meetings
    , scheduleMeetings
    , scheduleLitRevMeetings
      -- Interactions and timing
    , delay
    , request
    , getPainter
      -- Logging messages and errors
    , logMessage
    , logError
    ) where

import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Dates          as D
import qualified Model.Journals            as J
import qualified Model.Matching            as Mt
import qualified Model.Parsers.JournalSets as P
import qualified Model.Core.Types          as T
import qualified Data.Time                 as Tm
import qualified Data.Text                 as Tx
import qualified View.Core                 as Vc
import           Data.Text                          ( Text         )
import           Text.Read                          ( readMaybe    )
import           Control.Monad                      ( when         )
import           Control.Monad.Reader               ( asks         )
import           Control.Monad.Except               ( lift, liftIO
                                                    , throwError   )

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

readJSets :: FilePath -> T.AppMonad (T.JSets T.Issue)
-- ^Obtain a collection of journal sets from a file.
readJSets fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- references
    case P.parseJSets refs content of
         Left err    -> throwError err
         Right jsets -> pure jsets

getJSet :: T.HasIssue a => T.JSets a -> Maybe Int -> T.AppMonad (T.JSet a)
-- ^Find a single journal set from a collection of journal sets
-- consistent with any journal set key provided. If no key is
-- provided and there is only journal set in the collection then
-- return that single journal set.
getJSet (T.JSets []) _ = throwError "No journal sets!"
getJSet (T.JSets (j:[])) (Just k)
    | k == T.setNo j = pure j
    | otherwise      = throwError "Journal set does not match requested key!"
getJSet (T.JSets (j:[])) Nothing  = pure j
getJSet _     Nothing  = throwError "A valid journal set key is required!"
getJSet jsets (Just k) =
    let cannotFind = "Cannot find requested journal set!"
    in  case J.lookupJSet k jsets of
             Nothing   -> throwError cannotFind
             Just jset -> pure jset

getJSets :: T.HasIssue a => T.JSets a -> Maybe Int -> T.AppMonad (T.JSets a)
-- ^This is the same as getJSet, but returns a JSets collection. If
-- a key is provided, then the collection is restricted to the single
-- requested journal set. Otherwise, all the journal sets are returned.
getJSets (T.JSets []) _ = throwError "No journal sets!"
getJSets (T.JSets (j:[])) (Just k)
    | k == T.setNo j = pure . J.pack $ [j]
    | otherwise      = throwError "Journal set does not match requested key!"
getJSets (T.JSets (j:[])) Nothing  = pure . J.pack $ [j]
getJSets jsets Nothing  = pure jsets
getJSets jsets (Just k) =
    let cannotFind = "Cannot find requested journal set!"
    in  case J.lookupJSet k jsets of
             Nothing   -> throwError cannotFind
             Just jset -> pure . J.pack $ [jset]

-- =============================================================== --
-- Working with configured reference issues

references :: T.AppMonad T.References
references = asks T.cReferences

getIssue :: [String] -> T.AppMonad T.Issue
-- ^Try to find a configured issue based on the argument list and the
-- configuration. Throw an error if not possible.
getIssue ([]     ) = throwError $ "Journal abbreviation, year and issue number"
                                  <> " must be provided!"
getIssue (_:_:[] ) = throwError "The issue number must be provided!"
getIssue (_:[]   ) = throwError "The year and issue number must be provided!"
getIssue (i:y:n:_) = do
    rs <- references
    let abbr = Tx.pack i
    case (,) <$> readMaybe y <*> readMaybe n >>= J.lookupIssueInYear rs abbr of
         Just iss -> pure iss
         Nothing  -> throwError $ "Cannot find issue " <> n
                                  <> " from journal with abbreviation " <> i
                                  <> " in the year " <> y <> "!"

-- =============================================================== --
-- Rank matching

runMatch :: [(Text, [[Int]])] -> (Text, [Int]) -> T.AppMonad T.MatchResult
runMatch ranklists (title, indices) = pure $ Mt.match title indices ranklists

-- ================================================================== 
-- Scheduling Literature Review meetings

scheduleMeetings :: T.AppMonad [T.Meeting ()]
scheduleMeetings = do
    days  <- meetingDays
    pOne  <- asks T.cPresenterOne
    size  <- asks T.cMeetingSize
    pat   <- asks $ J.makePattern . T.cMeetPattern
    ps    <- asks $ J.groupPresenters size pOne . T.cPresenters
    count <- asks T.cMeetCount
    pure . take count
         . zipWith ( \ p m -> m { T.presenters = p } ) ps
         . J.assignMeetings pat $ days

scheduleLitRevMeetings :: T.JSets T.Issue
                          -> T.AppMonad [T.Meeting (T.JSet T.Issue) ]
scheduleLitRevMeetings jsets = do
    days  <- meetingDays
    pOne  <- asks T.cPresenterOne
    size  <- asks T.cMeetingSize
    pat   <- asks $ J.makePattern . T.cMeetPattern
    ps    <- asks $ J.groupPresenters size pOne . T.cPresenters
    count <- asks T.cMeetCount
    key   <- asks T.cJSetKey
    pure . take count
         . zipWith ( \ p m -> m { T.presenters = p } ) ps
         . J.assignDatedMeetings pat days
         . J.restrictJSets key $ jsets

meetingDays :: T.AppMonad [Tm.Day]
meetingDays = do
    skips <- asks T.cSkipDays
    today <- asks T.cDate
    start <- asks $ maybe today id . T.cStartDay
    pure . filter (not . flip elem skips) . iterate (Tm.addDays 7) $ start

-- =============================================================== --
-- Interactions and timing

addANSIseq :: T.AppMonad Bool
addANSIseq = do
    isTerm <- asks T.cStdOutIsTerm
    isANSI <- asks T.cUseANSI
    pure $ isTerm && isANSI

delay :: T.AppMonad ()
-- ^Cause a delay of at least 1 second in a sequenced AppMonad action.
-- The delay is specified by the cDelay configuration value.
delay = do duration <- asks $ (* 10^12) . T.cDelay
           useANSI  <- addANSIseq
           isTerm   <- asks T.cStdOutIsTerm
           when isTerm . logMessage $
               "Delay " <> Vc.showPicoSec duration <> " between requests..."
           liftIO . D.wait $ duration
           if | useANSI   -> logMessage "\ESC[2K\ESC[0G"
              | isTerm    -> logMessage "\n"
              | otherwise -> pure ()

request :: Text -> T.AppMonad Text
request msg = do
    isTerse <- asks T.cTerse
    isTerm  <- asks T.cStdOutIsTerm
    if ( not isTerse && isTerm )
       then do C.putTxtMIO msg
               Tx.strip . Tx.pack <$> liftIO getLine
       else pure Tx.empty

---------------------------------------------------------------------
-- colors & styling for terminal output

getPainter :: T.Color -> T.AppMonad (Text -> Text)
getPainter color = do
    useANSI <- addANSIseq
    if not useANSI
       then pure id
       else let go esc x = esc <> x <> "\ESC[0m"
            in  case color of
                     T.Red    -> pure $ go "\ESC[31m"
                     T.Green  -> pure $ go "\ESC[32m"
                     T.Yellow -> pure $ go "\ESC[33m"

-- =============================================================== --
-- Logging messages and recoverable error information

logMessage :: Text -> T.AppMonad ()
logMessage msg = do
    let msgNoEsc = Tx.map ( \ x -> if x == '\ESC' then 'E' else x ) msg
    isTerse <- asks T.cTerse
    isTerm  <- asks T.cStdOutIsTerm
    if | isTerse   -> pure ()
       | isTerm    -> C.putTxtMIO msg
       | otherwise -> C.putTxtMIO msgNoEsc

logError :: Text -> Text -> Text -> T.AppMonad ()
logError msg hdr err = do
    logPath <- asks T.cErrorLog
    paint   <- getPainter T.Red
    lift . C.logFileErr logPath $ "Error: " <> hdr <> "\n" <> err <> "\n"
    logMessage $ paint msg <> " (see " <> Tx.pack logPath <> ")\n"
