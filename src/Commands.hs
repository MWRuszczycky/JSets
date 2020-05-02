{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( -- Commands
      commands
    , runCommands
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.JournalSets as P
import qualified AppMonad                  as A
import           Data.Text                          ( Text           )
import           Data.List                          ( find           )
import           Text.Read                          ( readMaybe      )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , lift
                                                    , throwError     )

-- =============================================================== --
-- Commands

commands :: [ T.Command ]
commands = [ T.Command "refs"  refsCmd  refsHelp
           , T.Command "read"  readCmd  readHelp
           , T.Command "group" groupCmd groupHelp
           , T.Command "toc"   tocCmd   tocHelp
           , T.Command "year"  yearCmd  yearHelp
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
               , "option. For example, if <jsets2019.txt> is a file containing"
               , "all the journal sets for the year 2019, then you can print"
               , "journal set 5 to the terminal using,\n"
               , "    jsets read jsets2019.txt --key=5\n"
               , "The default output is formatted as text. You can use the"
               , "<read> command to convert between journal set formats. For"
               , "example, if you want to generate a csv file of the journal"
               , "sets use,\n"
               , "    jsets read jsets2019.txt --output=jsets2019.csv\n"
               , "The output format is determined by the output file extension."
               ]

readCmd :: [String] -> T.AppMonad ()
readCmd []     = throwError "A path to the journal sets file required!"
readCmd (fp:_) = do
    fmt   <- A.getFormat
    mbKey <- asks T.cJsetKey
    keys  <- A.issueRefKeys
    case (mbKey, fmt) of
         (Just _, T.CSV) -> A.jsetFromFile  fp >>= display . F.jsetToCsv keys
         (Just _, _    ) -> A.jsetFromFile  fp >>= display . F.jsetToTxt
         (_     , T.CSV) -> A.jsetsFromFile fp >>= display . F.jsetsToCsv keys
         (_     , _    ) -> A.jsetsFromFile fp >>= display . F.jsetsToTxt

---------------------------------------------------------------------
-- Construct journal set collections by year

yearHelp :: (Text, Text)
yearHelp = (s, Tx.unlines hs)
    where s  = "year : build a collection of all journal sets in a given year"
          hs = [ "The <year> command distributes all issues for all configured"
               , "journals in a given year into 26 journal sets. So, to create"
               , "a file with the default journal sets in 2019 use,\n"
               , "    jsets year 2019 --output=jsets2019.txt\n"
               , "The default output format is text. To format the file as csv,"
               , "set an output path with a 'csv' extension. For example,\n"
               , "    jsets year 2019 --output=jsets2019.csv\n"
               ]

yearCmd :: [String] -> T.AppMonad ()
yearCmd []    = throwError "A valid year must be specified!"
yearCmd (x:_) = do
    case readMaybe x of
         Nothing -> throwError "Invalid year."
         Just y  -> do jsets <- A.references >>= pure . J.yearly26Sets y
                       keys  <- A.issueRefKeys
                       fmt   <- A.getFormat
                       case fmt of
                            T.CSV -> display . F.jsetsToCsv keys $ jsets
                            _     -> display . F.jsetsToTxt $ jsets

---------------------------------------------------------------------
-- Handling issue selections

groupHelp :: (Text, Text)
groupHelp = (s, Tx.unlines hs)
    where s  = "group : group issue selects for review"
          hs = [ "Usage: jsets group file1.txt file2.txt file3.txt\n"
               , "Selection file formats are the same as journal set text files"
               , "with the first page of each selected article immediately"
               , "following the issue header line."
               ]

groupCmd :: [String] -> T.AppMonad ()
groupCmd []  = throwError "A selection file must be sepecified!"
groupCmd fps = do
    mbSel <- mapM readSelection fps >>= pure . J.groupSelections
    case mbSel of
         Nothing  -> throwError "No Issues in selection!"
         Just sel -> display . F.selectionToTxt $ sel

readSelection :: FilePath -> T.AppMonad T.SelectionSet
readSelection fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- A.references
    case P.parseSelection refs content of
         Left err  -> throwError err
         Right sel -> pure sel

---------------------------------------------------------------------
-- View configured journals

refsHelp :: (Text, Text)
refsHelp = (s, Tx.unlines hs)
    where s  = "refs : list configured journals and reference issues"
          hs = [ "Usage: jsets refs"
               ]

refsCmd :: [String] -> T.AppMonad ()
refsCmd _ = A.references >>= display . Tx.unlines . map F.referenceToTxt

---------------------------------------------------------------------
-- Download tables of contents for all issues in a journal set

tocHelp :: (Text, Text)
tocHelp = (s, Tx.unlines hs)
    where s  = "toc : generate tables of contents for a journal set"
          hs = [ "If jsets2019.txt is a collection of journal sets (see <read>"
               , "command), then you can generate a file of the tables of"
               , "contents for each issue in the set with key 6 using,\n"
               , "    jsets toc jsets2019.txt --key=6"
               , "The default output is text and will be printed to the"
               , "terminal. To generate the html output for easy selection, set"
               , "the extension of the output file to 'html', for example,\n"
               , "    jsets toc jsets2019.txt --key=6 --output=toc2019-6.html"
               ]

tocCmd :: [String] -> T.AppMonad ()
tocCmd []     = throwError "Path to the journal sets file is needed!"
tocCmd (fp:_) = do
    jset <- A.jsetFromFile fp
    tocs <- mapM A.downloadIssueToc . T.jsIssues $ jset
    fmt  <- A.getFormat
    case fmt of
         T.HTML -> display . F.tocsToMkd (T.jsKey jset) $ tocs
         _      -> display . F.tocsToTxt $ tocs

display :: Text -> T.AppMonad ()
display xs = do
    mbPath <- asks T.cOutputPath
    case mbPath of
         Nothing -> liftIO . Tx.putStrLn $ xs
         Just fp -> lift . C.writeFileErr fp $ xs
