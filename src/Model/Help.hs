{-# LANGUAGE OverloadedStrings #-}

module Model.Help
    ( helpText
    ) where

import qualified Data.Text             as Tx
import qualified System.Console.GetOpt as Opt
import qualified Model.Core.Types      as T
import           Data.Version                   ( showVersion )
import           Paths_lab_schedule             ( version     )
import           Data.Text                      ( Text        )

helpText :: [ Opt.OptDescr (T.Config -> T.Config) ] -> Text
helpText opts = Tx.unlines hs
    where hs = [ introHelp
               , optionsHelp opts
               , journalSetsHelp
               , tocHelp
               ]

versionHelp :: Text
versionHelp = Tx.pack $ "lab-schedule version " <> showVersion version

introHelp :: Text
introHelp = Tx.unlines hs
    where hs = [ versionHelp
               , "Management of journal sets and lab meeting schedules"
               ]

header :: Text -> Text
header x = Tx.unlines [ Tx.replicate 60 "-" , "-- " <> x ]

optionsHelp :: [Opt.OptDescr (T.Config -> T.Config)] -> Text
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"

journalSetsHelp :: Text
journalSetsHelp = Tx.unlines hs
    where hs = [ header "Working with journal sets"
               , "A JOURNAL SET is a set of journal issues that will be"
               , "reviewed at a literature review meeting. A journal set is"
               , "dated according to when all the issues in the set are"
               , "expected to be available in print such that each issue has a"
               , "defined year, volume and issue number. Each journal set also"
               , "has a KEY in the form of YEAR-NUMBER, which can be used to"
               , "reference it in a collection of journal sets.\n"
               , "Collections of journal sets can be read from files or"
               , "generated by year. The input file is set using the -i/--input"
               , "option and a relative path. The journal sets can be formatted"
               , "as comma separated values (csv) or text. If no input file is"
               , "provided, then the default journal sets for the year"
               , "specified by the -y/--year option is generated.\n"
               , "Once read or generated, the collection of journal sets can be"
               , "written to the file specified by the -o/--output option using"
               , "the format (txt, csv or mkd) set by the -f/--format option."
               , "The deafult output format is text. If no output path is"
               , "specified, the journal sets are sent to standard output."
               , "Output is in the parsable input format.\n"
               , "Example: Generate default journal sets for 2019 and print to"
               , "the terminal as easily readable text:\n"
               , "    lab-schedule -y2019\n"
               , "Example: Generate default journal sets for 2019 and save to"
               , "file jsets2019.csv as csv:\n"
               , "    lab-schedule -y2019 -fcsv -ojsets2019.csv\n"
               , "Example: Read journal sets from jsets2019.csv and print in"
               , "terminal as readable text:\n"
               , "    lab-schedule -ijsets2019.csv\n"
               , "Example: Read journal sets from jsets2019.csv and save as"
               , "readable markdown in the file jsets2019.mkd:\n"
               , "    lab-schedule -ijsets2019.csv -fmkd -ojsets2019.mkd\n"
               ]

tocHelp :: Text
tocHelp = Tx.unlines hs
    where hs = [ header "Working with tables of contents"
               , "Tables of contents for journal issues can be downloaded from"
               , "PubMed. To download the table of contents for all issues in"
               , "a journal set, a collection of journal sets must first be"
               , "read or generated and then the key of the specific journal"
               , "set of interest must be specified. Finally, the command"
               , "<toc> is used. Collections of journal sets are read or"
               , "generated as described above, and the journal set key is"
               , "specified usig the -k/--key option.\n"
               , "Example: Download the table of contents for the journal set"
               , "2019-3 in the default collection for year 2019 and save it"
               , "as the raw PubMed text to the file toc2019-3.txt:\n"
               , "    lab-schedule toc -y2019 -k2019-3 -otoc2019-3.txt\n"
               , "Example: Same as above, but use markdown instead of text:\n"
               , "    lab-schedule toc -y2019 -k2019-3 -fmkd -otoc2019-3.mkd\n"
               , "Example: Same as above, but use jsets2019.csv as the input:\n"
               , Tx.unwords $ [ "    lab-schedule toc -ijsets2019.csv -k2019-3"
                              , "-fmkd -otoc2019-3.mkd" ]
               ]
