{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runApp
    , configure
    ) where

import qualified System.Console.GetOpt     as Opt
import qualified Data.Text.IO              as Tx
import qualified Model.Core.Types          as T
import qualified Commands                  as Cmd
import qualified Model.Help                as H
import           System.Environment                 ( getArgs           )
import           Text.Read                          ( readMaybe         )
import           Data.List                          ( intercalate       )
import           Control.Monad                      ( foldM             )
import           Control.Monad.Except               ( throwError        )
import           Control.Monad.Reader               ( runReaderT
                                                    , liftIO            )

-- =============================================================== --
-- Main control point and routers

runApp :: ([String], T.Config) -> T.ErrMonad ()
runApp (cmds, config)
    | T.cHelp config = liftIO . Tx.putStrLn . H.helpText $ options
    | otherwise      = runReaderT ( route cmds ) config

route :: [String] -> T.AppMonad ()
route []          = pure ()
route ("toc":xs)  = Cmd.downloadJsetTocs xs >>= Cmd.finish
route ("year":xs) = Cmd.jsetsFromYear xs    >>= Cmd.finish
route ("read":xs) = Cmd.readJsetOrJsets xs
route (x:_)       = throwError $ "Unknown command: " <> x <> "\n"

-- =============================================================== --
-- Configuration

configure :: T.ErrMonad ([String], T.Config)
configure = do
    args <- liftIO getArgs
    case Opt.getOpt Opt.Permute options args of
         (fs, cs, [] ) -> foldM (flip ($)) initConfig fs >>= pure . ( (,) cs )
         (_,  _ , err) -> throwError . intercalate "\n" $ err

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.ErrMonad T.Config) ]
options =
    [ Opt.Option "o" [ "output" ]
      ( Opt.ReqArg ( \ arg s -> pure $ s { T.cOutputPath = Just arg } ) "PATH" )
      "Set the output filepath to PATH."

    , Opt.Option "f" [ "format" ]
      ( Opt.ReqArg configFormat "FMT" )
      "Set the output format to FMT (txt, csv, mkd)"

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cHelp = True } ) )
      "Display help information."

    , Opt.Option "k" [ "key" ]
      ( Opt.ReqArg configKey "KEY" )
      "Set the journal set key to KEY (positive integer)"
    ]

configKey :: String -> T.Config -> T.ErrMonad T.Config
configKey key config
    | n < 1     = throwError $ "Key must be a positive integer."
    | otherwise = pure $ config { T.cJsetKey = Just n }
    where n = maybe 0 id . readMaybe $ key

configFormat :: String -> T.Config -> T.ErrMonad T.Config
configFormat "csv" s = pure s { T.cFormat = Just T.CSV }
configFormat "mkd" s = pure s { T.cFormat = Just T.MKD }
configFormat "txt" s = pure s { T.cFormat = Just T.TXT }
configFormat x     _ = throwError $ "Unrecognized format: " <> x

initConfig :: T.Config
initConfig = T.Config { T.cOutputPath = Nothing
                      , T.cJsetKey    = Nothing
                      , T.cFormat     = Nothing
                      , T.cHelp       = False
                      }
