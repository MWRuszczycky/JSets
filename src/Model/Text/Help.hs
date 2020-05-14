{-# LANGUAGE OverloadedStrings #-}

module Model.Text.Help
    ( helpSummary
    , helpDetails
    , versionHelp
    ) where

import qualified Data.Text             as Tx
import qualified System.Console.GetOpt as Opt
import qualified Model.Core.Types      as T
import           Data.Version                   ( showVersion )
import           Paths_lab_schedule             ( version     )
import           Data.Text                      ( Text        )

-- =============================================================== --

type Option = Opt.OptDescr (T.Config -> T.ErrMonad T.Config)

helpSummary :: [T.Command] -> [Option] -> Text
helpSummary cmds opts = Tx.intercalate "\n" hs
    where hs = [ versionHelp
               , "Management of journal sets for lab meetings\n"
               , optionsHelp opts
               , "Commands summary:"
               , Tx.intercalate "\n" . map (("  " <>) . cmdSummary) $ cmds
               ]

versionHelp :: Text
versionHelp = Tx.pack $ "jsets-" <> showVersion version

cmdSummary :: T.Command -> Text
cmdSummary ( T.Command name _ (hs,_) ) = paddedName <> " : " <> hs
    where paddedName = Tx.pack name <> Tx.replicate (5 - length name) " "

helpDetails :: T.Command -> Text
helpDetails cmd = Tx.unlines [ cmdHeader cmd, snd . T.cmdHelp $ cmd ]

cmdHeader :: T.Command -> Text
cmdHeader cmd = Tx.replicate 60 "-" <> "\n-- " <> cmdSummary cmd <> "\n"

optionsHelp :: [Opt.OptDescr (T.Config -> T.ErrMonad T.Config)] -> Text
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"
