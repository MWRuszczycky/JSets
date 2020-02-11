{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    , finish
    , getSetup
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified System.Console.GetOpt     as Opt
import qualified Model.Core.Types          as T
import           Data.List                          ( foldl', intercalate )
import           Data.Default                       ( def                 )

-- =============================================================== --
-- Main control point and routers

controller :: T.Setup -> T.ErrMonad Tx.Text
controller _ = pure "Nothing to do"
    --let Just jset1 = Map.lookup (2019,1) . J.yearly26Sets 2019 $ R.issueRefs
    --writeToC "dev/tocs.mkd" ((2019,1), jset1)

finish :: Either String Tx.Text -> IO ()
finish (Left err)  = putStrLn $ err <> "Try option '-h' or '--help' for usage."
finish (Right msg) = Tx.putStrLn msg

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Setup -> T.Setup) ]
options =
    [ Opt.Option "d" [ "output-directory", "output-dir" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suOutputDir = Just arg } ) "DIR" )
      "Set the output-directory to DIR."

    , Opt.Option "h" [ "help", "info", "information" ]
      ( Opt.NoArg ( \ s -> s { T.suHelp = True } ) )
      "Display help information."

    , Opt.Option "s" [ "jset", "journal-set" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suJsetKey = Just arg } ) "KEY" )
      "Set the journal set key to KEY"

    , Opt.Option "y" [ "year" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suJsetYear = Just arg } ) "YEAR" )
      "Set the year for the journal sets to YEAR."
    ]

getSetup :: [String] -> Either T.ErrString T.Setup
getSetup xs =
    case Opt.getOpt Opt.Permute options xs of
         (fs,cs,[] ) -> pure $ foldl' (flip ($)) (def { T.suCommands = cs }) fs
         (_ ,_ ,err) -> Left . intercalate "\n" $ err
