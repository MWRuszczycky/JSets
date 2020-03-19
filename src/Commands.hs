{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( -- Commands
      commands
    , runCommands
      -- Handling output
    , finish
      -- Data structer construction & aquisition
    , jsetsFromFile
    , jsetFromFile
      -- General helper functions
    , getFormat
    , requireKey
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Core           as C
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.PubMed      as P
import qualified Model.Parsers.JournalSets as P
import qualified Model.Core.References     as R
import           Data.Text                          ( Text           )
import           Data.Maybe                         ( isJust         )
import           Data.List                          ( find           )
import           Text.Read                          ( readMaybe      )
import           System.IO                          ( hFlush, stdout )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , lift
                                                    , throwError
                                                    , liftEither     )

-- =============================================================== --
-- Commands

commands :: [ T.Command ]
commands = [ T.Command "read"   readJsetOrJsets  readHelp
           , T.Command "year"   jsetsFromYear    yearHelp
           , T.Command "select" handleSelection  selectHelp
           , T.Command "toc"    downloadJsetTocs tocHelp
           ]

runCommands :: [String] -> T.AppMonad ()
runCommands []     = pure ()
runCommands (x:xs) = maybe err go . find ( (==x) . T.cmdName ) $ commands
    where go  = flip T.cmdAction xs
          err = throwError $ "Unknown command: " <> x

---------------------------------------------------------------------
-- File format reading and conversion

readHelp :: (Text, Text)
readHelp = (s, Tx.unlines hs)
    where s  = "read : read journal sets from file"
          hs = [ "A journal set is a collection of journal issues for review."
               , "Collections of journal sets can be read from a file in either"
               , "text or csv format using the <read> command. A specific set"
               , "can be read alone by specifying the key (a positive integer)"
               , "of the specific journal set of interest using the --key/-k"
               , "option. Collections of journal sets can be generated using"
               , "the <year> command. For example, if <jsets2019.txt> is a file"
               , "containing all the journal sets for 2019, then you can print"
               , "journal set 5 to the terminal using,\n"
               , "    lab-schedule read jsets2019.txt --key=5\n"
               , "You can use the <read> command to convert between journal set"
               , "formats. For example, if you want to generate a csv file of"
               , "the journal sets use,\n"
               , "    lab-schedule read jsets2019.txt --output=jsets2019.csv\n"
               , "The output format is determined by the output file extension;"
               , "however, it can be over-ridden using the --format/-f command."
               , "The default output format is text."
               ]

readJsetOrJsets :: [String] -> T.AppMonad ()
readJsetOrJsets []     = throwError "A path to the journal sets file required!"
readJsetOrJsets (fp:_) = do
    keyProvided <- asks $ isJust . T.cJsetKey
    if keyProvided
       then jsetFromFile fp  >>= finish
       else jsetsFromFile fp >>= finish

---------------------------------------------------------------------
-- Construct journal set collections by year

yearHelp :: (Text, Text)
yearHelp = (s, Tx.unlines hs)
    where s  = "year : build a collection of all journal sets in a given year"
          hs = [ "The <year> command distributes all issues for all configured"
               , "journals in a given year into 26 journal sets. So, to create"
               , "a file with the default journal sets in 2019 use,\n"
               , "    lab-schedule year 2019 --output=jsets2019.txt\n"
               , "You can also create a csv file by changing the extension to"
               , ".csv or using the --format/-f option."
               ]

jsetsFromYear :: [String] -> T.AppMonad ()
jsetsFromYear []    = throwError "A valid year must be specified!"
jsetsFromYear (x:_) = maybe err go  (readMaybe x) >>= finish
    where err  = throwError "Invalid year."
          go y = pure . T.Result [C.tshow y] . J.yearly26Sets y $ R.issueRefs

---------------------------------------------------------------------
-- Handling issue selections

selectHelp :: (Text, Text)
selectHelp = (s, Tx.unlines hs)
    where s  = "select : collect issues selected for review"
          hs = [ "Usage:\n"
               , "    lab-schedule select file1.txt file2.txt file3.txt\n"
               , "Selection file formats are the same as journal set text files"
               , "with the first page of each selected article immediately"
               , "following the issue header line."
               ]

handleSelection :: [String] -> T.AppMonad ()
handleSelection []  = throwError "A selection file must be sepecified!"
handleSelection fps = mapM readSelection fps
                      >>= pure . J.groupSelections
                      >>= maybe (throwError "No Issues in selection!") pure
                      >>= finish . T.Result R.issueRefKeys

readSelection :: FilePath -> T.AppMonad T.SelectionSet
readSelection fp = lift (C.readFileErr fp) >>= liftEither . P.parseSelection

---------------------------------------------------------------------
-- Download tables of contents for all issues in a journal set

tocHelp :: (Text, Text)
tocHelp = (s, Tx.unlines hs)
    where s  = "toc : generate tables of contents for a journal set"
          hs = [ "If jsets2019.txt is a collection of journal sets (see <read>"
               , "command), then you can generate a markdown file of the tables"
               , "of contents for each issue in the set with key 6 using,\n"
               , "    lab-schedule toc jsets2019.txt --key=6"
                 <> " --output=toc2019-5.mkd\n"
               , "Note that the default output format is text and will be"
               , "printed to the terminal. So, you need to either specify an"
               , "output path with the desired extension and/or use the"
               , "--format/-f option with 'mkd' or 'md' to indicate markdown."
               ]

downloadJsetTocs :: [String] -> T.AppMonad ()
downloadJsetTocs []     = throwError "Path to the journal sets file is needed!"
downloadJsetTocs (fp:_) = do
    jset <- T.result <$> jsetFromFile fp
    tocs <- mapM downloadIssueToc . T.jsIssues $ jset
    finish $ T.Result [C.tshow $ T.jsKey jset]
             $ T.JSetToC (T.jsKey jset) tocs

-- =============================================================== --
-- Handling output

finish :: F.Formattable a => T.Result a -> T.AppMonad ()
finish (T.Result hdr x) = do
    fmt  <- getFormat
    path <- asks T.cOutputPath
    case path of
         Nothing -> liftIO . Tx.putStrLn . F.format fmt hdr $ x
         Just fp -> lift . C.writeFileErr fp . F.format fmt hdr $ x

-- =============================================================== --
-- Data structure construction & acquisition

---------------------------------------------------------------------
-- Acquiring journal sets

jsetsFromFile :: FilePath -> T.AppMonad (T.Result T.JournalSets)
jsetsFromFile fp = lift ( C.readFileErr fp )
                   >>= liftEither . P.parseJsets
                   >>= pure . T.Result R.issueRefKeys

---------------------------------------------------------------------
-- Read a single journal set from a file

jsetFromFile :: FilePath -> T.AppMonad (T.Result T.JournalSet)
-- ^Get a journal set based on the configuration.
jsetFromFile fp = do
    jsets <- T.result <$> jsetsFromFile fp
    key   <- requireKey
    case J.lookupJSet key jsets of
         Nothing   -> throwError "Cannot find requested journal set."
         Just jset -> pure $ T.Result R.issueRefKeys jset

---------------------------------------------------------------------
-- Downloading issue table of contents

downloadIssueToc ::  T.Issue -> T.AppMonad T.IssueToC
-- ^Download the table of contents for a journal issue from PubMed.
-- Display progress messages for each ToC download.
downloadIssueToc x = do
    let addr = "https://www.ncbi.nlm.nih.gov/pubmed"
        opts = J.tocQuery x
    liftIO . Tx.putStr $ "Downloading toc for " <> F.issueToTxt x <> "..."
    liftIO . hFlush $ stdout
    cs <- lift $ C.webRequest opts addr >>= liftEither . P.parseCitations x
    if null cs
       then liftIO . Tx.putStrLn $ "No articles listed at PubMed"
       else liftIO . Tx.putStrLn $ "OK"
    pure $ T.IssueToC x cs

-- =============================================================== --
-- General Helper Commands

getFormat :: T.AppMonad T.Format
getFormat = do
    mbFmt <- asks T.cFormat
    path  <- asks T.cOutputPath
    case ( mbFmt, C.extension <$> path ) of
         (Just fmt, _         ) -> pure fmt
         (Nothing , Just "txt") -> pure T.TXT
         (Nothing , Just "md" ) -> pure T.MKD
         (Nothing , Just "mkd") -> pure T.MKD
         (Nothing , Just "csv") -> pure T.CSV
         _                      -> pure T.TXT

requireKey :: T.AppMonad Int
requireKey = asks T.cJsetKey >>= maybe err pure
    where err = throwError "A valid journal set key is required."
