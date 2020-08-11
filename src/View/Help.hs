{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module View.Help
    ( summary
    , cmdDetails
    , jsetsDetails
    , version
    , versionStr
      -- Help strings
    , helpHelp
    , jsonHelp
    , matchHelp
    , ranksHelp
    , readHelp
    , refsHelp
    , tocHelp
    , yearHelp
    ) where

-- =============================================================== --
-- Help strings and output formatting
-- =============================================================== --

import qualified Data.Text             as Tx
import qualified System.Console.GetOpt as Opt
import qualified Data.FileEmbed        as FE
import qualified Model.Core.Types      as T
import qualified Model.Core.Core       as M
import qualified Data.Version          as Ver
import qualified Paths_jsets           as Paths
import           Data.Text                      ( Text        )

-- =============================================================== --
-- Local types

type Option = Opt.OptDescr (T.Config -> T.ErrMonad T.Config)

-- =============================================================== --
-- Interface

summary :: [T.Command] -> [Option] -> Text
-- ^General summary of all commands and options available. This
-- string is displayed when the user runs JSets with the --help flag.
summary cmds opts = Tx.intercalate "\n" hs
    where hs = [ versionStr
               , sep
               , "Management of journal sets for lab meetings."
               , "  (Run 'jsets help jsets' for more information.)\n"
               , optionsHelp opts
               , "Commands summary:"
               , Tx.intercalate "\n" . map (("  " <>) . cmdSummary) $ cmds
               ]

cmdDetails :: T.Command -> Text
-- ^Detailed help for a given command.
cmdDetails cmd = Tx.intercalate "\n" [ versionStr, cmdHeader cmd, hs ]
    where hs = snd . T.cmdHelp $ cmd

jsetsDetails :: Text
-- ^Detailed description of the the JSets application.
jsetsDetails = Tx.intercalate "\n" [ versionStr, sep <> "\n", jsetsHelp ]

version :: T.Version
version = let go = Tx.intercalate "." . map M.tshow
          in  case Ver.versionBranch Paths.version of
                   []   -> T.DevVersion $ "jsets-0.0"
                   0:[] -> T.DevVersion $ "jsets-0.0"
                   0:xs -> T.DevVersion $ "jsets-" <> go xs
                   xs   -> T.RelVersion $ "jsets-" <> go xs

versionStr :: Text
-- ^Current version of the application. If the version starts with
-- with a leading 0, then it is a development version.
versionStr = case version of
                  T.DevVersion x -> "post-" <> x <> " development version"
                  T.RelVersion x -> x

-- =============================================================== --
-- Formatting helper functions

cmdSummary :: T.Command -> Text
-- ^Construct a one-line description of the command.
cmdSummary ( T.Command name _ (hs,_) ) = paddedName <> " : " <> hs
    where paddedName = Tx.pack name <> Tx.replicate (5 - length name) " "

cmdHeader :: T.Command -> Text
-- ^Construct a summary header for detailed command help.
cmdHeader cmd = sep <> "\n  " <> cmdSummary cmd <> "\n"

optionsHelp :: [Opt.OptDescr (T.Config -> T.ErrMonad T.Config)] -> Text
-- ^Construct a summary of all command line options.
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"

sep :: Text
sep = Tx.replicate 20 "-"

-- =============================================================== --
-- Help strings

helpHelp :: Text
helpHelp = $(FE.embedStringFile "res/help/helpHelp.txt")

jsetsHelp :: Text
jsetsHelp = $(FE.embedStringFile "res/help/jsetsHelp.txt")

jsonHelp :: Text
jsonHelp = $(FE.embedStringFile "res/help/jsonHelp.txt")

matchHelp :: Text
matchHelp = $(FE.embedStringFile "res/help/matchHelp.txt")

ranksHelp :: Text
ranksHelp = $(FE.embedStringFile "res/help/ranksHelp.txt")

readHelp :: Text
readHelp = $(FE.embedStringFile "res/help/readHelp.txt")

refsHelp :: Text
refsHelp = $(FE.embedStringFile "res/help/refsHelp.txt")

tocHelp :: Text
tocHelp = $(FE.embedStringFile "res/help/tocHelp.txt")

yearHelp :: Text
yearHelp = $(FE.embedStringFile "res/help/yearHelp.txt")
