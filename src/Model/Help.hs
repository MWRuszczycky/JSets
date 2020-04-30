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
helpText cmds opts = Tx.intercalate "\n" hs
    where hs = [ introHelp
               , optionsHelp opts
               , Tx.intercalate "\n" . map commandsHelp $ cmds
               ]

introHelp :: Text
introHelp = Tx.unlines hs
    where hs = [ Tx.pack $ "jsets-" <> showVersion version
               , "Management of journal sets for lab meetings"
               ]

optionsHelp :: [Opt.OptDescr (T.Config -> T.ErrMonad T.Config)] -> Text
optionsHelp = Tx.pack . Opt.usageInfo "Options summary:"

commandsHelp :: T.Command -> Text
commandsHelp c = let (s,l) = T.cmdHelp c
                 in  Tx.intercalate "\n" [ Tx.replicate 60 "-"
                                           <> "\n-- " <> s <> "\n"
                                         , l ]
