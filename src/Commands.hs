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
           , T.Command "issue" issueCmd issueHelp
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
-- Downloading a single issue from pubmed

issueHelp :: (Text, Text)
issueHelp = (s, H.issueHelp)
    where s = "Download the table of contents for a configured journal issue."

issueCmd :: [String] -> T.AppMonad ()
issueCmd args = do
    x    <- A.getIssue args
    wreq <- PM.getWreqSession
    (,) <$> asks T.cFormat <*> asks T.cOnlyPMIDs >>= \case
         ( T.JSON, True ) -> queryESearchJSON wreq x
         ( _,      True ) -> PM.getPMIDs wreq x >>= display . Tx.unlines
         ( T.JSON, _    ) -> PM.getPMIDs wreq x >>= queryESummaryJSON wreq
         _                -> PM.getPMIDs wreq x >>= queryCitations wreq

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
pmidCmd xs = PM.getWreqSession >>= flip queryCitations pmids
    where pmids = map Tx.pack xs

---------------------------------------------------------------------
-- Submit a query to PubMed

queryHelp :: (Text, Text)
queryHelp = (s, "TODO")
    where s = "Submit a query to PubMed"

queryCmd :: [String] -> T.AppMonad ()
queryCmd = undefined

queryESearchJSON :: T.CanQuery a => C.WebRequest -> a -> T.AppMonad ()
queryESearchJSON wreq x = PM.eSearch wreq x >>= \case
    Right json -> display json
    Left  err  -> do paint <- A.getPainter "red"
                     let msg = paint "Failed!"
                         hdr = "Cannot complete eSearch request:"
                     A.logError msg hdr . Tx.pack $ err

queryESummaryJSON :: C.WebRequest -> [T.PMID] -> T.AppMonad ()
queryESummaryJSON wreq pmids = PM.eSummary wreq pmids >>= \case
    Right json -> display json
    Left  err  -> do paint <- A.getPainter "red"
                     let msg = paint "Failed!"
                         hdr = "Cannot complete eSummary request:"
                     A.logError msg hdr . Tx.pack $ err

queryCitations :: C.WebRequest -> [T.PMID] -> T.AppMonad ()
queryCitations wreq pmids = do
    cs   <- PM.getCitations wreq pmids
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
