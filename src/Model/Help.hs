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

-- =============================================================== --

type Option = Opt.OptDescr (T.Config -> T.ErrMonad T.Config)

helpText :: [T.Command] -> [Option] -> Text
helpText _ opts = Tx.unlines hs
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

optionsHelp :: [Opt.OptDescr (T.Config -> T.ErrMonad T.Config)] -> Text
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"

journalSetsHelp :: Text
journalSetsHelp = Tx.unlines hs
    where hs = [ header "Working with journal sets"
               , "To generate a journal set collection by year, use\n"
               , "    lab-schedule year YEAR\n"
               , "where YEAR is the year of interest. Output format is set by"
               , "either the output file path (use the --output/-o option) or"
               , "the --format/-f option."
               ]

tocHelp :: Text
tocHelp = Tx.unlines hs
    where hs = [ header "Working with tables of contents"
               , "Tables of contents for journal issues can be downloaded from"
               , "PubMed. This requires that a journal set collection file be"
               , "available. This collection file can be generated using the"
               , "<year> command with the csv format and then edited. The"
               , "journal set of interest is specified by its key using the"
               , "--key/-k option. Output format is as described above. Thus,"
               , "to generate a toc file for the third journal set in the"
               , "collection described by the file jsets2019.csv, use\n"
               , "    lab-schedule toc jsets2019.csv -k3 -otoc2019-3.mkd"
               ]
