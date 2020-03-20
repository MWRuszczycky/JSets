{-# LANGUAGE OverloadedStrings #-}
module Commands
    ( -- Commands
      commands
    , runCommands
      -- Handling output
    , finish
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Model.Core.Core           as C
import qualified Model.Journals            as J
import qualified Model.Formatting          as F
import qualified Model.Parsers.JournalSets as P
import qualified AppMonad                  as A
import           Data.Text                          ( Text           )
import           Data.Maybe                         ( isJust         )
import           Data.List                          ( find           )
import           Text.Read                          ( readMaybe      )
import           Control.Monad.Reader               ( asks           )
import           Control.Monad.Except               ( liftIO
                                                    , lift
                                                    , throwError     )

-- =============================================================== --
-- Commands

commands :: [ T.Command ]
commands = [ T.Command "list"   listCmd   listHelp
           , T.Command "read"   readCmd   readHelp
           , T.Command "select" selectCmd selectHelp
           , T.Command "toc"    tocCmd    tocHelp
           , T.Command "year"   yearCmd   yearHelp
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

readCmd :: [String] -> T.AppMonad ()
readCmd []     = throwError "A path to the journal sets file required!"
readCmd (fp:_) = do
    keyProvided <- asks $ isJust . T.cJsetKey
    if keyProvided
       then A.jsetFromFile fp  >>= finish
       else A.jsetsFromFile fp >>= finish

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

yearCmd :: [String] -> T.AppMonad ()
yearCmd []    = throwError "A valid year must be specified!"
yearCmd (x:_) = maybe err go (readMaybe x) >>= finish
    where err  = throwError "Invalid year."
          go y = A.references >>= pure . T.Result [C.tshow y] . J.yearly26Sets y

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

selectCmd :: [String] -> T.AppMonad ()
selectCmd []  = throwError "A selection file must be sepecified!"
selectCmd fps = do
    mbSel <- mapM readSelection fps >>= pure . J.groupSelections
    case mbSel of
         Nothing  -> throwError "No Issues in selection!"
         Just sel -> A.issueRefKeys >>= finish . flip T.Result sel

readSelection :: FilePath -> T.AppMonad T.SelectionSet
readSelection fp = do
    content <- lift . C.readFileErr $ fp
    refs    <- A.references
    case P.parseSelection refs content of
         Left err  -> throwError err
         Right sel -> pure sel

---------------------------------------------------------------------
-- View configured journals

listHelp :: (Text, Text)
listHelp = (s, Tx.unlines hs)
    where s  = "list : list configured journals and reference issues"
          hs = [ "Usage:\n"
               , "    lab-schedule list\n"
               ]

listCmd :: [String] -> T.AppMonad ()
listCmd _ = A.references >>= display . map F.referenceToTxt
    where display = liftIO . Tx.putStr . Tx.unlines

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

tocCmd :: [String] -> T.AppMonad ()
tocCmd []     = throwError "Path to the journal sets file is needed!"
tocCmd (fp:_) = do
    jset <- T.result <$> A.jsetFromFile fp
    tocs <- mapM A.downloadIssueToc . T.jsIssues $ jset
    finish $ T.Result [C.tshow $ T.jsKey jset]
             $ T.JSetToC (T.jsKey jset) tocs

-- =============================================================== --
-- Handling output

finish :: F.Formattable a => T.Result a -> T.AppMonad ()
finish (T.Result hdr x) = do
    fmt  <- A.getFormat
    path <- asks T.cOutputPath
    case path of
         Nothing -> liftIO . Tx.putStrLn . F.format fmt hdr $ x
         Just fp -> lift . C.writeFileErr fp . F.format fmt hdr $ x
