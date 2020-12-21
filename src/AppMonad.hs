{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}

module AppMonad
    ( -- Data structer construction & aquisition
      readJSets
    , getJSet
    , getJSets
      -- Working with configured reference issues
    , references
    , getIssue
      -- Running rank matchings
    , runMatch
      -- Interactions and timing
    , delay
    , request
    , getPainter
      -- Logging messages and errors
    , logMessage
    , logError
    ) where

import qualified Data.Text                 as Tx
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Types          as T
import qualified Model.Core.Dates          as D
import qualified Model.Journals            as J
import qualified Model.Matching            as Mt
import qualified Model.Parsers.JournalSets as P
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

-- =============================================================== --
-- Interactions and timing

delay :: T.AppMonad ()
-- ^Cause a delay of at least 1 second in a sequenced AppMonad action.
-- The delay is specified by the cDelay configuration value.
delay = do duration <- asks $ (* 10^12) . T.cDelay
           isTerm   <- asks T.cStdOutIsTerm
           when isTerm . logMessage $
               "Delay " <> Vc.showPicoSec duration <> " between requests.."
           liftIO . D.wait $ duration
           when isTerm . logMessage $ "\ESC[2K\ESC[0G"

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
    isTerm <- asks T.cStdOutIsTerm
    let go esc x = esc <> x <> "\ESC[0m"
    case ( isTerm, color    ) of
         ( False,  _        ) -> pure   id
         ( _,      T.Red    ) -> pure $ go "\ESC[31m"
         ( _,      T.Green  ) -> pure $ go "\ESC[32m"
         ( _,      T.Yellow ) -> pure $ go "\ESC[33m"

-- =============================================================== --
-- Logging messages and recoverable error information

logMessage :: Text -> T.AppMonad ()
logMessage msg = do
    let msgNoEsc = Tx.map ( \ x -> if x == '\ESC' then 'E' else x ) msg
    isTerse <- asks T.cTerse
    isTerm  <- asks T.cStdOutIsTerm
    case ( isTerse, isTerm ) of
         ( True,    _      ) -> pure ()
         ( _,       True   ) -> C.putTxtMIO msg
         ( _,       _      ) -> C.putTxtMIO msgNoEsc

logError :: Text -> Text -> Text -> T.AppMonad ()
logError msg hdr err = do
    logPath <- asks T.cErrorLog
    paint   <- getPainter T.Red
    lift . C.logFileErr logPath $ "Error: " <> hdr <> "\n" <> err <> "\n"
    logMessage $ paint msg <> " (see " <> Tx.pack logPath <> ")\n"
