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
import qualified Commands                  as C
import           Data.List                          ( foldl', intercalate )
import           Data.Default                       ( def                 )
import           Control.Monad.Except               ( throwError          )

-- =============================================================== --
-- Main control point and routers

controller :: T.Setup -> T.ErrMonad Tx.Text
controller su = case T.runMode su of
                     T.NoMode      -> pure "Nothing to do"
                     T.HelpMode    -> pure "Display help"
                     T.ToCMode     -> C.writeTocsToMkd su
                     T.ErrMode err -> throwError err

finish :: Either String Tx.Text -> IO ()
finish (Right msg) = Tx.putStrLn msg
finish (Left err)  = putStr $ unlines [ err, msg ]
    where msg = "Try option '-h' or '--help' for usage."

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Setup -> T.Setup) ]
options =
    [ Opt.Option "d" [ "output-directory", "output-dir" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suOutputDir = Just arg } ) "DIR" )
      "Set the output-directory to DIR."

    , Opt.Option "h" [ "help", "info", "information" ]
      ( Opt.NoArg ( \ s -> s { T.runMode = T.HelpMode } ) )
      "Display help information."

    , Opt.Option "k" [ "jset", "key", "journal-set" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suJsetKey = Just arg } ) "KEY" )
      "Set the journal set key to KEY"

    , Opt.Option "x" [ "jsets", "journal-sets" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suJsetsFile = Just arg } ) "PATH" )
      "Set filepath for the journal sets to PATH."

    , Opt.Option "y" [ "year" ]
      ( Opt.ReqArg ( \ arg s -> s { T.suJsetsYear = Just arg } ) "YEAR" )
      "Set the year for the journal sets to YEAR."
    ]

getSetup :: [String] -> Either T.ErrString T.Setup
getSetup xs =
    case Opt.getOpt Opt.Permute options xs of
         (fs,cs,[] ) -> pure . setMode cs . foldl' (flip ($)) def $ fs
         (_ ,_ ,err) -> Left . intercalate "\n" $ err

setMode :: [String] -> T.Setup -> T.Setup
setMode []         su = su
setMode ("toc":xs) su = su { T.runMode = T.ToCMode,     T.suCmds = xs }
setMode cs         su = su { T.runMode = T.ErrMode err, T.suCmds = cs }
    where err = "Invalid usage."
