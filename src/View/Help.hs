{-# LANGUAGE OverloadedStrings #-}

module View.Help
    ( summary
    , details
    , version
    ) where

import qualified Data.Text             as Tx
import qualified System.Console.GetOpt as Opt
import qualified Model.Core.Types      as T
import qualified Data.Version          as Ver
import qualified Paths_lab_schedule    as Paths
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
cmdHeader cmd = Tx.replicate 60 "-" <> "\n-- " <> cmdSummary cmd <> "\n"

optionsHelp :: [Opt.OptDescr (T.Config -> T.ErrMonad T.Config)] -> Text
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"
