{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Commands
    ( -- Commands
      commands
    , runCommand
    ) where

import qualified Data.Map.Strict           as Map
import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified AppMonad                  as A
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Journals            as J
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.Rankings    as P
import qualified PubMed                    as PM
import qualified View.View                 as V
import qualified View.Help                 as H
import           Data.Text                          ( Text           )
import           Data.List                          ( find           )
import           Text.Read                          ( readMaybe      )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO, lift
                                                    , liftEither
                                                    , throwError     )

-- =============================================================== --
-- Commands

commands :: [ T.Command ]
-- ^Commands should not be more than five characters long.
commands = [ T.Command "help"  helpCmd  helpHelp
           , T.Command "json"  jsonCmd  jsonHelp
           , T.Command "match" matchCmd matchHelp
           , T.Command "pmid"  pmidCmd  pmidHelp
           , T.Command "ranks" ranksCmd ranksHelp
           , T.Command "read"  readCmd  readHelp
           , T.Command "refs"  refsCmd  refsHelp
           , T.Command "toc"   tocCmd   tocHelp
           , T.Command "year"  yearCmd  yearHelp
           ]

runCommand :: [String] -> T.AppMonad ()
runCommand []     = pure ()
runCommand (x:xs) = maybe err go . find ( (==x) . T.cmdName ) $ commands
    where go  = flip T.cmdAction xs
          err = throwError $ "Unknown command: " <> x

---------------------------------------------------------------------
-- Providing help information

helpHelp :: (Text, Text)
helpHelp = (s, H.helpHelp)
    where s = "Display help information for a command (e.g., help refs)."

helpCmd :: [String] -> T.AppMonad ()
helpCmd []          = helpCmd ["help"]
helpCmd ("jsets":_) = display H.jsetsDetails
helpCmd (c:_)       = maybe err go . find ( (==c) . T.cmdName ) $ commands
    where err = throwError $ "Unknown command: " <> c
          go  = display . H.cmdDetails

---------------------------------------------------------------------
-- Downloading raw json from pubmed

jsonHelp :: (Text, Text)
jsonHelp = (s, H.jsonHelp)
    where s = "Download raw PubMed json responses for a journal issue."

jsonCmd :: [String] -> T.AppMonad ()
jsonCmd [] = throwError "A journal issue must be specified."
jsonCmd xs
    | length xs < 3 = throwError "Invalid number of arguments (should be 3)!"
    | otherwise     = do
        let abbr = Tx.pack $ xs !! 0
        v <- maybe (throwError "invalid volume!")   pure . readMaybe $ xs !! 1
        n <- maybe (throwError "invalid number!") pure . readMaybe $ xs !! 2
        query <- PM.eSearchQuery =<< A.getIssue abbr v n
        esearch <- lift . C.webRequest query $ PM.eSearchUrl
        lift . C.writeFileErr "esearch.json" $ esearch
        pmids <- liftEither . P.parsePMIDs $ esearch
        esummary <- lift . C.webRequest (PM.eSummaryQuery pmids) $ PM.eSummaryUrl
        lift . C.writeFileErr "esummary.json" $ esummary

---------------------------------------------------------------------
-- Match ranking for distributing papers

matchHelp :: (Text, Text)
matchHelp = (s, H.matchHelp)
    where s = "Perform matchings according to a rank-lists file."

matchCmd :: [String] -> T.AppMonad ()
matchCmd []     = throwError "A path to a match file must be provided!"
matchCmd (fp:_) = do
    (ks, rs) <- (lift . C.readFileErr) fp >>= liftEither . P.parseRankings
    results  <-  mapM (A.runMatch rs) ks
    mapM_ ( \ r -> V.runView (V.viewMatchResult r) >>= display ) results

---------------------------------------------------------------------
-- Look up PMIDs at PubMed

pmidHelp :: (Text, Text)
pmidHelp = (s, H.pmidHelp)
    where s = "Download citations based on ther PMIDs."

pmidCmd :: [String] -> T.AppMonad ()
pmidCmd [] = throwError "One or more PMIDs must be provided!"
pmidCmd xs = do
    wreq <- PM.getWreqSession
    cs   <- PM.getCitations wreq . map Tx.pack $ xs
    rs   <- A.references
    let citations = map (J.resolveCitationIssue rs) . Map.elems $ cs
    V.runView ( V.viewCitations citations ) >>= display

---------------------------------------------------------------------
-- Generating output for ranking articles

ranksHelp :: (Text, Text)
ranksHelp = (s, H.ranksHelp)
    where s = "Output a list of selected articles for ranking."

ranksCmd :: [FilePath] -> T.AppMonad ()
ranksCmd [] = throwError "Path(s) to journal set selection files required!"
ranksCmd fps = do
    jsets     <- J.combineJSets <$> mapM A.readJSets fps
    jset      <- asks T.cJSetKey >>= A.getJSet jsets
    wreq      <- PM.getWreqSession
    citations <- PM.getCitations wreq . J.pmidsInSelection . T.selection $ jset
    A.logMessage "Done\n"
    V.runView ( V.viewRanks citations jset ) >>= display

---------------------------------------------------------------------
-- File reading and conversion

readHelp :: (Text, Text)
readHelp = (s, H.readHelp)
    where s = "Read journal sets from file."

readCmd :: [FilePath] -> T.AppMonad ()
readCmd []  = throwError "Path(s) to journal sets files are required!"
readCmd fps = do
    combined <- J.combineJSets <$> mapM A.readJSets fps
    jsets    <- A.getJSets combined =<< asks T.cJSetKey
    V.runView ( V.viewJSets jsets ) >>= display

---------------------------------------------------------------------
-- View configured journals

refsHelp :: (Text, Text)
refsHelp = (s, H.refsHelp)
    where s = "List configured journals and reference issues."

refsCmd :: [String] -> T.AppMonad ()
refsCmd _ = V.runView  V.viewConfig >>= display

---------------------------------------------------------------------
-- Download tables of contents for all issues in a journal set

tocHelp :: (Text, Text)
tocHelp = (s, H.tocHelp)
    where s = "Generate tables of contents for a journal set."

tocCmd :: [FilePath] -> T.AppMonad ()
tocCmd []  = throwError "Path(s) to journal sets files are required!"
tocCmd fps = do
    jsets     <- J.combineJSets <$> mapM A.readJSets fps
    (cs,jset) <- asks T.cJSetKey >>= A.getJSet jsets >>= PM.getToCs
    V.runView ( V.viewToCs cs jset ) >>= display

---------------------------------------------------------------------
-- Construct journal set collections by year

yearHelp :: (Text, Text)
yearHelp = (s, H.yearHelp)
    where s = "Build a collection of journal sets for a given year."

yearCmd :: [String] -> T.AppMonad ()
yearCmd []      = throwError "A valid year must be specified!"
yearCmd (y:[])  = yearCmd [ y, "2" ]
yearCmd (y:w:_) = do
    theYear <- checkYear y
    theFreq <- checkFreq w
    jsets   <- A.references >>= pure . J.yearlySets theYear theFreq
    V.runView ( V.viewJSets jsets ) >>= display

checkYear :: String -> T.AppMonad Int
checkYear y = do
    theYear <- maybe (throwError "Invalid YEAR.") pure . readMaybe $ y
    minYear <- A.references >>= \case
                   [] -> throwError "No references specified."
                   rs -> pure . maximum . map T.year $ rs
    if minYear <= theYear && theYear <= 2100
       then pure theYear
       else throwError $ unwords [ "YEAR must be between"
                                 , show minYear
                                 , "and 2100 inclusive."
                                 ]

checkFreq :: String -> T.AppMonad Int
checkFreq w = do
    theFreq <- maybe (throwError "Invalid FREQ.") pure . readMaybe $ w
    if theFreq < 1 || theFreq > 52
       then throwError "FREQ must be between 1 and 52 inclusive."
       else pure theFreq

---------------------------------------------------------------------
-- Output handling

display :: Text -> T.AppMonad ()
display xs = asks T.cOutputPath >>= \case
                 Nothing -> liftIO . Tx.putStr $ xs
                 Just fp -> lift . C.writeFileErr fp $ xs
