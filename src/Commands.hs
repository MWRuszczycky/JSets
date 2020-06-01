{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Commands
    ( -- Commands
      commands
    , runCommands
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified AppMonad                  as A
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Journals            as J
import qualified Model.Parsers.PubMed      as P
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
           , T.Command "ranks" ranksCmd ranksHelp
           , T.Command "read"  readCmd  readHelp
           , T.Command "refs"  refsCmd  refsHelp
           , T.Command "toc"   tocCmd   tocHelp
           , T.Command "year"  yearCmd  yearHelp
           ]

runCommands :: [String] -> T.AppMonad ()
runCommands []     = pure ()
runCommands (x:xs) = maybe err go . find ( (==x) . T.cmdName ) $ commands
    where go  = flip T.cmdAction xs
          err = throwError $ "Unknown command: " <> x

---------------------------------------------------------------------
-- Providing help information

helpHelp :: (Text, Text)
helpHelp = (s, H.helpHelp)
    where s  = "display help information for a command (e.g., help refs)"

helpCmd :: [String] -> T.AppMonad ()
helpCmd []    = helpCmd ["help"]
helpCmd (c:_) = maybe err go . find ( (==c) . T.cmdName ) $ commands
    where err = throwError $ "Unknown command: " <> c
          go  = display . H.details

---------------------------------------------------------------------
-- Downloading raw json from pubmed

jsonHelp :: (Text, Text)
jsonHelp = (s, H.jsonHelp)
    where s  = "download raw PubMed json responses for a journal issue"

jsonCmd :: [String] -> T.AppMonad ()
jsonCmd [] = throwError "A journal issue must be specified."
jsonCmd xs
    | length xs < 3 = throwError "Invalid number of arguments (should be 3)!"
    | otherwise     = do
        let abbr = Tx.pack $ xs !! 0
        v   <- maybe (throwError "invalid volume!")   pure . readMaybe $ xs !! 1
        n   <- maybe (throwError "invalid number!") pure . readMaybe $ xs !! 2
        iss <- A.getIssue abbr v n
        esearch <- lift . C.webRequest (J.tocESearchQuery iss) $ J.eSearchUrl
        lift . C.writeFileErr "esearch.json" $ esearch
        pmids <- liftEither . P.parsePMIDs $ esearch
        esummary <- lift . C.webRequest (J.tocESumQuery pmids) $ J.eSummaryUrl
        lift . C.writeFileErr "esummary.json" $ esummary

---------------------------------------------------------------------
-- Generating output for ranking articles

ranksHelp :: (Text, Text)
ranksHelp = (s, H.ranksHelp)
    where s  = "output a list of selected articles generating a ranklist"

ranksCmd :: [FilePath] -> T.AppMonad ()
ranksCmd [] = throwError "Path(s) to journal set selection files required!"
ranksCmd fps = do
    jsets        <- J.combineJSets <$> mapM A.readJSets fps
    T.JSet n sel <- asks T.cJSetKey >>= A.getJSet jsets
    citations    <- A.downloadCitations C.webRequest (concatMap T.selected sel)
    C.putStrLnMIO "\nDone"
    let (ics, cs) = J.addContent citations sel
        jset      = T.JSet n ics
    checkForUnrequestedCitations cs
    checkForMissingCitations ics
    V.runView ( V.viewRanks jset ) >>= display

checkForUnrequestedCitations :: [T.Citation] -> T.AppMonad ()
-- ^These are citations that were downloaded from PubMed but not were
-- not requested as part of the selection. In principle, there should
-- never be any unrequested citations if PubMed is working correctly.
-- These will not be included in the constructed content. We just
-- to let the user know that something weird happened.
checkForUnrequestedCitations [] = pure ()
checkForUnrequestedCitations cs = do
    C.putTxtMIO "There are unrequested citations returned by PubMed, PMIDs: "
    C.putTxtLnMIO . Tx.unwords . map T.pmid $ cs
    C.putTxtMIO "  These have not been included in the ranks list output."

checkForMissingCitations :: [T.Content] -> T.AppMonad ()
-- ^Check to make sure each pmid in the selection for each issue has
-- a corresponding citation. Otherwise, the citation is missing. Note
-- that there will never be citations without corresponding pmids.
-- These will always end up as unrequested citations, because only
-- the requested citations are incorporated into the issue contents.
checkForMissingCitations ics
    | null pmids = pure ()
    | otherwise  = C.putTxtLnMIO msg
    where pmids = concatMap J.missingPMIDs ics
          msg   = "There are missing citations, PMIDS: " <> Tx.unwords pmids

---------------------------------------------------------------------
-- File reading and conversion

readHelp :: (Text, Text)
readHelp = (s, H.readHelp)
    where s  = "read journal sets from file"

readCmd :: [FilePath] -> T.AppMonad ()
readCmd []  = throwError "Path(s) to journal sets files are required!"
readCmd fps = do
    combined <- J.combineJSets <$> mapM A.readJSets fps
    jsets    <- A.getJSets combined =<< asks T.cJSetKey
    V.runView ( V.viewSelections jsets ) >>= display

---------------------------------------------------------------------
-- View configured journals

refsHelp :: (Text, Text)
refsHelp = (s, H.refsHelp)
    where s  = "list configured journals and reference issues"

refsCmd :: [String] -> T.AppMonad ()
refsCmd _ = do
    rs <- map V.referenceToTxt <$> A.references
    p  <- asks T.cRefPath
    let hdr = Tx.pack $ "References file: " <> p <> "\n"
    display . Tx.intercalate "\n" $ hdr : rs

---------------------------------------------------------------------
-- Download tables of contents for all issues in a journal set

tocHelp :: (Text, Text)
tocHelp = (s, H.tocHelp)
    where s  = "generate tables of contents for a journal set"

tocCmd :: [FilePath] -> T.AppMonad ()
tocCmd []  = throwError "Path(s) to journal sets files are required!"
tocCmd fps = do
    jsets       <- J.combineJSets <$> mapM A.readJSets fps
    T.JSet n ss <- asks T.cJSetKey >>= A.getJSet jsets
    jset        <- A.downloadContents ss >>= pure . T.JSet n
    V.runView ( V.viewToCs jset ) >>= display

---------------------------------------------------------------------
-- Construct journal set collections by year

yearHelp :: (Text, Text)
yearHelp = (s, H.yearHelp)
    where s  = "build a collection of all journal sets in a given year"

yearCmd :: [String] -> T.AppMonad ()
yearCmd []    = throwError "A valid year must be specified!"
yearCmd (x:_) = do
    theYear <- maybe (throwError "Invalid year.") pure . readMaybe $ x
    jsets   <- A.references >>= pure . J.yearly26Sets theYear
    V.runView ( V.viewJSetsIssue jsets ) >>= display

---------------------------------------------------------------------
-- Output handling

display :: Text -> T.AppMonad ()
display xs = asks T.cOutputPath >>= \case
                 Nothing -> liftIO . Tx.putStr $ xs
                 Just fp -> lift . C.writeFileErr fp $ xs
