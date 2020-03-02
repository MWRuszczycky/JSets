{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runApp
    , configure
    ) where

import qualified Data.Text.IO              as Tx
import qualified System.Console.GetOpt     as Opt
import qualified Model.Core.Types          as T
import qualified Model.Core.CoreIO         as C
import qualified Commands                  as Cmd
import qualified Model.Help                as H
import qualified Model.Formatting          as F
import           Data.Text                          ( Text                )
import           System.Environment                 ( getArgs             )
import           Text.Read                          ( readMaybe           )
import           Data.List                          ( foldl', intercalate )
import           Data.Default                       ( def                 )
import           Control.Monad.Except               ( throwError
                                                    , ExceptT (..)        )
import           Control.Monad.Reader               ( runReaderT, asks
                                                    , liftIO, lift        )

-- =============================================================== --
-- Main control point and routers

configure :: T.ErrMonad ([String], T.Config)
configure = ExceptT $ argsToConfig <$> getArgs

runApp :: ([String], T.Config) -> T.ErrMonad ()
runApp (cmds, config)
    | T.cHelp config = liftIO . Tx.putStrLn . H.helpText $ options
    | otherwise      = runReaderT ( route cmds ) config

route :: [String] -> T.AppMonad ()
route []          = pure ()
route ("toc":xs)  = Cmd.getTocs xs >>= finish
route ("year":xs) = Cmd.jsetsFromYear xs >>= finish
route (x:_)       = throwError $ "Unknown command: " <> x <> "\n"

finish :: F.Formattable a => T.Result a -> T.AppMonad ()
finish (T.Result hdr x) = do
    fmt  <- asks T.cFormat
    path <- asks T.cOutputPath
    case path of
         Nothing -> liftIO . Tx.putStrLn . F.format fmt hdr $ x
         Just fp -> lift . C.writeFileErr fp . F.format fmt hdr $ x

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.Config) ]
options =
    [ Opt.Option "o" [ "output" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cOutputPath = Just arg } ) "PATH" )
      "Set the output filepath to PATH."

    , Opt.Option "f" [ "format" ]
      ( Opt.ReqArg configFormat "FMT" )
      "Set the output format to FMT (txt, csv, mkd)"

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg ( \ s -> s { T.cHelp = True } ) )
      "Display help information."

    , Opt.Option "k" [ "key" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cJsetKey = readMaybe arg } ) "KEY" )
      "Set the journal set key to KEY (positive integer)"
    ]

argsToConfig :: [String] -> Either T.ErrString ([String], T.Config)
argsToConfig xs =
    case Opt.getOpt Opt.Permute options xs of
         (fs,cs,[] ) -> pure ( cs, foldl' (flip ($)) def $ fs )
         (_ ,_ ,err) -> Left . intercalate "\n" $ err

configFormat :: String -> T.Config -> T.Config
configFormat "csv" s = s { T.cFormat = T.CSV }
configFormat "mkd" s = s { T.cFormat = T.MKD }
configFormat "txt" s = s { T.cFormat = T.TXT }
configFormat _     s = s
