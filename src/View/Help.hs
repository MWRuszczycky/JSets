{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module View.Help
    ( summary
    , details
    , version
      -- Help strings
    , helpHelp
    , jsonHelp
    , ranksHelp
    , readHelp
    , refsHelp
    , tocHelp
    , yearHelp
    ) where

import qualified Data.Text             as Tx
import qualified System.Console.GetOpt as Opt
import qualified Data.FileEmbed        as FE
import qualified Model.Core.Types      as T
import qualified Data.Version          as Ver
import qualified Paths_jsets           as Paths
import           Data.Text                      ( Text        )

-- =============================================================== --

type Option = Opt.OptDescr (T.Config -> T.ErrMonad T.Config)

summary :: [T.Command] -> [Option] -> Text
summary cmds opts = Tx.intercalate "\n" hs
    where hs = [ version
               , "Management of journal sets for lab meetings\n"
               , optionsHelp opts
               , "Commands summary:"
               , Tx.intercalate "\n" . map (("  " <>) . cmdSummary) $ cmds
               ]

version :: Text
version = Tx.pack $ "jsets-" <> Ver.showVersion Paths.version

details :: T.Command -> Text
details cmd = Tx.intercalate "\n" [ version
                                  , cmdHeader cmd
                                  , snd . T.cmdHelp $ cmd ]

cmdSummary :: T.Command -> Text
cmdSummary ( T.Command name _ (hs,_) ) = paddedName <> " : " <> hs
    where paddedName = Tx.pack name <> Tx.replicate (5 - length name) " "

cmdHeader :: T.Command -> Text
cmdHeader cmd = Tx.replicate 20 "-" <> "\n  " <> cmdSummary cmd <> "\n"

optionsHelp :: [Opt.OptDescr (T.Config -> T.ErrMonad T.Config)] -> Text
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"

-- =============================================================== --
-- Help strings

helpHelp :: Text
helpHelp = $(FE.embedStringFile "res/help/helpHelp.txt")

jsonHelp :: Text
jsonHelp = $(FE.embedStringFile "res/help/jsonHelp.txt")

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
