{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runApp
    , configure
    ) where

import qualified System.Console.GetOpt     as Opt
import qualified Data.Text.IO              as Tx
import qualified Model.Core.Types          as T
import qualified Model.Text.Help           as H
import           System.Environment                 ( getArgs               )
import           Text.Read                          ( readMaybe             )
import           Data.List                          ( intercalate           )
import           Control.Monad                      ( foldM                 )
import           Control.Monad.Except               ( throwError            )
import           Control.Monad.Reader               ( runReaderT, liftIO    )
import           Commands                           ( runCommands, commands )
import           Model.Core.References              ( issueRefs             )

-- =============================================================== --
-- Main control point and routers

runApp :: ([String], T.Config) -> T.ErrMonad ()
runApp (cmds, config)
    | T.cHelp config = liftIO . Tx.putStrLn . H.helpText commands $ options
    | otherwise      = runReaderT ( runCommands cmds ) config

-- =============================================================== --
-- Configuration

configure :: T.ErrMonad ([String], T.Config)
configure = do
    args <- liftIO getArgs
    case Opt.getOpt Opt.Permute options args of
         (fs, cs, [] ) -> foldM (flip ($)) initConfig fs >>= pure . ( (,) cs )
         (_,  _ , err) -> throwError . intercalate "\n" $ err

initConfig :: T.Config
initConfig = T.Config { T.cOutputPath = Nothing
                      , T.cJsetKey    = Nothing
                      , T.cHelp       = False
                      , T.cReferences = issueRefs
                      }

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.ErrMonad T.Config) ]
options =
    [ Opt.Option "o" [ "output" ]
      ( Opt.ReqArg ( \ arg s -> pure $ s { T.cOutputPath = Just arg } ) "PATH" )
      "Set the output filepath to PATH."

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
