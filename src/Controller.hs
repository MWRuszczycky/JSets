{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    , finish
    , configure
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

controller :: T.Config -> T.ErrMonad Tx.Text
controller c = case T.runMode c of
                    T.NoMode      -> pure "Nothing to do"
                    T.HelpMode    -> pure "Display help"
                    T.ToCMode     -> C.writeTocsToMkd c
                    T.ErrMode err -> throwError err

finish :: Either String Tx.Text -> IO ()
finish (Right msg) = Tx.putStrLn msg
finish (Left err)  = putStr $ unlines [ err, msg ]
    where msg = "Try option '-h' or '--help' for usage."

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.Config) ]
options =
    [ Opt.Option "d" [ "output-directory", "output-dir" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cOutputDir = Just arg } ) "DIR" )
      "Set the output-directory to DIR."

    , Opt.Option "h" [ "help", "info", "information" ]
      ( Opt.NoArg ( \ s -> s { T.runMode = T.HelpMode } ) )
      "Display help information."

    , Opt.Option "k" [ "jset", "key", "journal-set" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cJsetKey = Just arg } ) "KEY" )
      "Set the journal set key to KEY"

    , Opt.Option "x" [ "jsets", "journal-sets" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cJsetsFile = Just arg } ) "PATH" )
      "Set filepath for the journal sets to PATH."

    , Opt.Option "y" [ "year" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cJsetsYear = Just arg } ) "YEAR" )
      "Set the year for the journal sets to YEAR."
    ]

configure :: [String] -> Either T.ErrString T.Config
configure xs =
    case Opt.getOpt Opt.Permute options xs of
         (fs,cs,[] ) -> pure . setMode cs . foldl' (flip ($)) def $ fs
         (_ ,_ ,err) -> Left . intercalate "\n" $ err

setMode :: [String] -> T.Config -> T.Config
setMode []         c = c
setMode ("toc":xs) c = c { T.runMode = T.ToCMode,     T.cCmds = xs   }
setMode cmds       c = c { T.runMode = T.ErrMode err, T.cCmds = cmds }
    where err = "Invalid usage."
