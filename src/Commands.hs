{-# LANGUAGE OverloadedStrings #-}
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
commands = [ T.Command "group" groupCmd groupHelp
           , T.Command "help"  helpCmd  helpHelp
           , T.Command "read"  readCmd  readHelp
           , T.Command "refs"  refsCmd  refsHelp
           , T.Command "json"  jsonCmd  jsonHelp
           , T.Command "toc"   tocCmd   tocHelp
           , T.Command "year"  yearCmd  yearHelp
           ]

runCommands :: [String] -> T.AppMonad ()
runCommands []     = pure ()
runCommands (x:xs) = maybe err go . find ( (==x) . T.cmdName ) $ commands
    where go  = flip T.cmdAction xs
          err = throwError $ "Unknown command: " <> x

---------------------------------------------------------------------
-- Grouping multiple issue selections

groupHelp :: (Text, Text)
groupHelp = (s, Tx.unlines hs)
    where s  = "group issue selections for review"
          hs = [ "Usage: jsets group file1.txt file2.txt file3.txt\n"
               , "Selection file formats are the same as journal set text files"
               , "with the first page of each selected article immediately"
               , "following the issue header line."
               ]

groupCmd :: [String] -> T.AppMonad ()
groupCmd []  = throwError "A selection file must be sepecified!"
groupCmd fps = do
    mbSel <- mapM A.readJset fps >>= pure . J.groupJsets
    case mbSel of
         Nothing  -> throwError "No Issues in selection!"
         Just sel -> display . V.selectionToTxt $ sel

---------------------------------------------------------------------
-- Downloading raw json from pubmed

jsonHelp :: (Text, Text)
jsonHelp = (s, Tx.unlines hs)
    where s  = "download raw json for a journal issue (for debugging)"
          hs = [ "Usage: json <Journal-abbreviation> <year> <issue>"
               , "Generates two files:"
               , "    esearch.json:  the esearch response"
               , "    esummary.json: the esummary response"
               ]

jsonCmd :: [String] -> T.AppMonad ()
jsonCmd [] = throwError "A journal issue must be specified."
jsonCmd xs
    | length xs < 3 = throwError "Invalid number of arguments (should be 3)!"
    | otherwise     = do
        let abbr = Tx.pack $ xs !! 0
        y   <- maybe (throwError "invalid year!")   pure . readMaybe $ xs !! 1
        n   <- maybe (throwError "invalid number!") pure . readMaybe $ xs !! 2
        iss <- A.getIssue abbr y n
        esearch <- lift . C.webRequest (J.tocESearchQuery iss) $ J.eSearchUrl
        lift . C.writeFileErr "esearch.json" $ esearch
        pmids <- liftEither . P.parsePMIDs $ esearch
        esummary <- lift . C.webRequest (J.tocESumQuery pmids) $ J.eSummaryUrl
        lift . C.writeFileErr "esummary.json" $ esummary

---------------------------------------------------------------------
-- Grouping multiple issue selections

helpHelp :: (Text, Text)
helpHelp = (s, Tx.unlines hs)
    where s  = "display help information for a command (e.g., help refs)"
          hs = [ "Usage: help <command-name>"
               , "Displays detailed information for a command."
               , "To see a help information summary and complete list of"
               , "available commands, use the --help/-h flag."
               ]

helpCmd :: [String] -> T.AppMonad ()
helpCmd []    = helpCmd ["help"]
helpCmd (c:_) = maybe err go . find ( (==c) . T.cmdName ) $ commands
    where err = throwError $ "Unknown command: " <> c
          go  = display . H.details

---------------------------------------------------------------------
-- File format reading and conversion

readHelp :: (Text, Text)
readHelp = (s, Tx.unlines hs)
    where s  = "read journal sets from file"
          hs = [ "A journal set is a collection of journal issues for review."
               , "Collections of journal sets can be read from a file in either"
               , "text or csv format using the <read> command. A specific set"
               , "can be read alone by specifying the key (a positive integer)"
               , "of the specific journal set of interest using the --key/-k"
               , "option. For example, if <jsets2019.txt> is a file containing"
               , "all the journal sets for the year 2019, then you can print"
               , "journal set 5 to the terminal using,\n"
               , "    jsets read jsets2019.txt --key=5\n"
               , "The default output is formatted as text. You can use the"
               , "<read> command to convert between journal set formats by"
               , "using an explicit output path and indicating the format with"
               , "the output file extension. So, if you want to convert a text"
               , "file to csv, then you would run,\n"
               , "    jsets read jsets2019.txt --output=jsets2019.csv\n"
               , "If you want to convert the csv file to text, then run,\n"
               , "    jsets read jsets2019.csv --output=jsets2019.txt"
               ]

readCmd :: [String] -> T.AppMonad ()
readCmd []     = throwError "A path to the journal sets file required!"
readCmd (fp:_) = do
    fmt   <- A.getFormat
    mbKey <- asks T.cJsetKey
    abbrs <- A.issueRefAbbrs
    case (mbKey, fmt) of
         (Just _, T.CSV) -> A.readJset  fp >>= display . V.jsetToCsv abbrs
         (Just _, T.MKD) -> A.readJset  fp >>= display . V.jsetToMkd
         (Just _, _    ) -> A.readJset  fp >>= display . V.jsetToTxt
         (_     , T.CSV) -> A.readJsets fp >>= display . V.jsetsToCsv abbrs
         (_     , T.MKD) -> A.readJsets fp >>= display . V.jsetsToMkd
         (_     , _    ) -> A.readJsets fp >>= display . V.jsetsToTxt

---------------------------------------------------------------------
-- View configured journals

refsHelp :: (Text, Text)
refsHelp = (s, Tx.unlines hs)
    where s  = "list configured journals and reference issues"
          hs = [ "Usage: jsets refs"
               ]

refsCmd :: [String] -> T.AppMonad ()
refsCmd _ = do
    rs <- map V.referenceToTxt <$> A.references
    p  <- asks T.cRefPath
    let hdr = Tx.pack $ "References file: " <> p <> "\n"
    display . Tx.unlines $ hdr : rs

---------------------------------------------------------------------
-- Download tables of contents for all issues in a journal set

tocHelp :: (Text, Text)
tocHelp = (s, Tx.unlines hs)
    where s  = "generate tables of contents for a journal set"
          hs = [ "If jsets2019.txt is a collection of journal sets (see <read>"
               , "command), then you can generate a file of the tables of"
               , "contents for each issue in the set with key 6 using,\n"
               , "    jsets toc jsets2019.txt --key=6\n"
               , "The default output is text and will be printed to the"
               , "terminal. To generate the html output for easy selection, set"
               , "the extension of the output file to 'html', for example,\n"
               , "    jsets toc jsets2019.txt --key=6 --output=toc2019-6.html\n"
               , "The tables of contents are downloaded from PubMed. To obtain"
               , "the raw, unparsed PubMed data, use the extension 'raw',"
               , "for example,\n"
               , "    jsets toc jsets2019.txt --key=6 --output=toc2019-6.raw\n"
               , "Markdown formatting is available with the 'mkd' extension,\n"
               , "    jsets toc jsets2019.txt --key=6 --output=toc2019-6.mkd\n"
               , "When reading from a selection file you need to set a style"
               , "flag to indicate that a selection file is being parsed and"
               , "the type of table of contents information to generate:\n"
               , "    --select/-s : toc for selecting citations for review"
               , "    --rank/-r   : toc for ranking citations"
               ]

tocCmd :: [String] -> T.AppMonad ()
tocCmd []     = throwError "Path to the journal sets file is needed!"
tocCmd (fp:_) = do
    T.JSet n iss <- A.readJset fp
    ics   <- mapM A.downloadPubMed iss
    style <- asks T.cToCStyle
    fmt   <- A.getFormat
    case fmt of
         T.HTML -> display . V.tocsToHtml style . T.JSet n $ ics
         T.MKD  -> display . V.tocsToMkd        . T.JSet n $ ics
         _      -> display . V.tocsToTxt        . T.JSet n $ ics

---------------------------------------------------------------------
-- Construct journal set collections by year

yearHelp :: (Text, Text)
yearHelp = (s, Tx.unlines hs)
    where s  = "build a collection of all journal sets in a given year"
          hs = [ "The <year> command distributes all issues for all configured"
               , "journals in a given year into 26 journal sets. So, to create"
               , "a file with the default journal sets in 2019 use,\n"
               , "    jsets year 2019 --output=jsets2019.txt\n"
               , "The default output format is text. To format the file as csv,"
               , "set an output path with a 'csv' extension. For example,\n"
               , "    jsets year 2019 --output=jsets2019.csv"
               ]

yearCmd :: [String] -> T.AppMonad ()
yearCmd []    = throwError "A valid year must be specified!"
yearCmd (x:_) = do
    theYear <- maybe (throwError "Invalid year.") pure . readMaybe $ x
    jsets   <- A.references >>= pure . J.yearly26Sets theYear
    abbrs   <- A.issueRefAbbrs
    fmt     <- A.getFormat
    case fmt of
         T.CSV -> display . V.jsetsToCsv abbrs $ jsets
         _     -> display . V.jsetsToTxt $ jsets

---------------------------------------------------------------------
-- Output handling

display :: Text -> T.AppMonad ()
display xs = do
    mbPath <- asks T.cOutputPath
    case mbPath of
         Nothing -> liftIO . Tx.putStr $ xs
         Just fp -> lift . C.writeFileErr fp $ xs
