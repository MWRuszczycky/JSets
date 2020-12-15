{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module View.Help
    ( summary
    , cmdDetails
    , jsetsDetails
    , version
      -- Help strings
    , helpHelp
    , jsonHelp
    , matchHelp
    , pmidHelp
    , ranksHelp
    , readHelp
    , refsHelp
    , tocHelp
    , yearHelp
    ) where

-- =============================================================== --
-- Help strings and output formatting
-- =============================================================== --

import qualified Model.Core.CoreTH     as MT
import qualified Model.Core.Types      as T
import qualified Data.Text             as Tx
import qualified Data.Version          as Ver
import qualified Paths_jsets           as Paths
import qualified System.Console.GetOpt as Opt
import           Data.Text                      ( Text )

-- =============================================================== --
-- Local types

type Option = Opt.OptDescr T.ConfigStep

-- =============================================================== --
-- Interface

summary :: [T.Command] -> [Option] -> Text
-- ^General summary of all commands and options available. This
-- string is displayed when the user runs JSets with the --help flag.
summary cmds opts = Tx.intercalate "\n" hs
    where hs = [ version
               , sep
               , "Management of journal sets for lab meetings."
               , "  (Run 'jsets help jsets' for more information.)\n"
               , optionsHelp opts
               , "Commands summary:"
               , Tx.intercalate "\n" . map (("  " <>) . cmdSummary) $ cmds
               ]

cmdDetails :: T.Command -> Text
-- ^Detailed help for a given command.
cmdDetails cmd = Tx.intercalate "\n" [ version, cmdHeader cmd, hs ]
    where hs = snd . T.cmdHelp $ cmd

jsetsDetails :: Text
-- ^Detailed description of the the JSets application.
jsetsDetails = Tx.intercalate "\n" [ version, sep <> "\n", jsetsHelp ]

version :: Text
version = "jsets-" <> v <> "-" <> g
    where v = Tx.pack . Ver.showVersion $ Paths.version
          g = Tx.take 7 $(MT.readGitHash)

-- =============================================================== --
-- Formatting helper functions

cmdSummary :: T.Command -> Text
-- ^Construct a one-line description of the command.
cmdSummary ( T.Command name _ (hs,_) ) = paddedName <> " : " <> hs
    where paddedName = Tx.pack name <> Tx.replicate (5 - length name) " "

cmdHeader :: T.Command -> Text
-- ^Construct a summary header for detailed command help.
cmdHeader cmd = sep <> "\n  " <> cmdSummary cmd <> "\n"

optionsHelp :: [Option] -> Text
-- ^Construct a summary of all command line options.
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"

sep :: Text
sep = Tx.replicate 20 "-"

-- =============================================================== --
-- Help strings

helpHelp :: Text
helpHelp = $(MT.embedFile "res/help/helpHelp.txt")

jsetsHelp :: Text
jsetsHelp = $(MT.embedFile "res/help/jsetsHelp.txt")

jsonHelp :: Text
jsonHelp = $(MT.embedFile "res/help/jsonHelp.txt")

matchHelp :: Text
matchHelp = $(MT.embedFile "res/help/matchHelp.txt")

pmidHelp :: Text
pmidHelp = $(MT.embedFile "res/help/pmidHelp.txt")

ranksHelp :: Text
ranksHelp = $(MT.embedFile "res/help/ranksHelp.txt")

readHelp :: Text
readHelp = $(MT.embedFile "res/help/readHelp.txt")

refsHelp :: Text
refsHelp = $(MT.embedFile "res/help/refsHelp.txt")

tocHelp :: Text
tocHelp = $(MT.embedFile "res/help/tocHelp.txt")

yearHelp :: Text
yearHelp = $(MT.embedFile "res/help/yearHelp.txt")
