{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runApp
    , configure
    ) where

import qualified Data.Text.IO          as Tx
import qualified Data.Text             as Tx
import qualified Model.Core.Types      as T
import qualified Model.Core.CoreIO     as C
import qualified Model.Core.Core       as C
import qualified Model.Core.Dates      as Cd
import qualified Model.Parsers.Config  as P
import qualified System.Console.GetOpt as Opt
import qualified View.Help             as H
import           Commands                     ( runCommands, commands  )
import           Control.Monad                ( foldM                  )
import           Control.Monad.Except         ( throwError             )
import           Control.Monad.Reader         ( runReaderT, liftIO     )
import           Data.List                    ( intercalate            )
import           Data.Text                    ( pack                   )
import           System.Directory             ( getHomeDirectory
                                              , doesFileExist          )
import           System.Environment           ( getArgs                )
import           Text.Read                    ( readMaybe              )

-- =============================================================== --
-- Main control point and routers

runApp :: ([String], T.Config) -> T.ErrMonad ()
runApp (cmds, config)
    | T.cHelp config    = liftIO . Tx.putStrLn . H.summary commands $ options
    | T.cShowVer config = liftIO . Tx.putStrLn $ H.version
    | otherwise         = runReaderT ( runCommands cmds ) config

-- =============================================================== --
-- Configuration

configure :: T.ErrMonad ([String], T.Config)
configure = initConfig >>= configFromCommandLine >>= configFromFile

initConfig :: T.ErrMonad T.Config
initConfig = do
    path   <- ( <> "/.config/jsets/config" ) <$> liftIO getHomeDirectory
    exists <- liftIO . doesFileExist $ path
    today  <- liftIO Cd.today
    if exists
       then pure $ T.defaultConfig { T.cRefPath = Just path, T.cDate = today }
       else pure $ T.defaultConfig { T.cDate = today }

configFromCommandLine :: T.Config -> T.ErrMonad ([String], T.Config)
configFromCommandLine config = do
    args <- liftIO getArgs
    case Opt.getOpt Opt.Permute options args of
         (fs, cs, [] ) -> foldM (flip ($)) config fs >>= pure . ( (,) cs )
         (_,  _ , err) -> throwError . intercalate "\n" $ err

configFromFile :: ([String], T.Config) -> T.ErrMonad ([String], T.Config)
configFromFile (cmds, config) = do
    content <- maybe (pure Tx.empty) C.readFileErr . T.cRefPath $ config
    case P.parseConfig content >>= P.readConfig config of
         Left  pErr    -> configError (T.cRefPath config) pErr
         Right config' -> pure ( cmds , config' )

configError :: Maybe FilePath -> T.ErrString -> T.ErrMonad a
configError Nothing     _   = throwError "Missing configuration file."
configError (Just path) err = throwError msg
    where msg = "Unable to parse configuration file " <> path <> "\n" <> err

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.ErrMonad T.Config) ]
options =
    [
    -- Parameters

      Opt.Option "" [ "user" ]
      ( Opt.ReqArg ( \ x s -> pure $ s { T.cUser = Just . pack $ x } ) "USER" )
      "Override the configured user name with USER."

    , Opt.Option "" [ "email" ]
      ( Opt.ReqArg ( \ x s -> pure $ s { T.cEmail = Just . pack $ x } ) "EMAIL" )
      "Override the configured email with EMAIL."

    , Opt.Option "c" [ "config" ]
      ( Opt.ReqArg ( \ x s -> pure $ s { T.cRefPath = Just x } ) "PATH" )
      "Use the configuration file at path PATH."

    , Opt.Option "k" [ "key" ]
      ( Opt.ReqArg configKey "KEY" )
      "Set the journal set key to KEY (positive integer)."

    , Opt.Option "o" [ "output" ]
      ( Opt.ReqArg ( \ x s -> pure $ s { T.cOutputPath = Just x } ) "PATH" )
      "Set the output filepath to PATH."

    , Opt.Option "" [ "delay" ]
      ( Opt.ReqArg configDelay "SEC" )
      "Delay in whole seconds between PubMed requests."

    , Opt.Option "" [ "fmt" ]
      ( Opt.ReqArg configFormat "FMT" )
      "Set the output format to FMT overriding file extension."

    -- Flags

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cHelp = True } ) )
      "Display help information."

    , Opt.Option "v" [ "version" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cShowVer = True } ) )
      "Just show the version number and quit."

    , Opt.Option "" [ "no-sort" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cSortJSets = False } ) )
      "Do not sort journal set issues."

    , Opt.Option "" [ "match-details" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cMatchDetails = True } ) )
      "Provide detailed match output."
    ]

configKey :: String -> T.Config -> T.ErrMonad T.Config
configKey key config
    | n < 1     = throwError $ "Key must be a positive integer."
    | otherwise = pure $ config { T.cJSetKey = Just n }
    where n = maybe 0 id . readMaybe $ key

configDelay :: String -> T.Config -> T.ErrMonad T.Config
configDelay delay config
    | d < 1     = throwError $ "Delay time must be a positive integer."
    | otherwise = pure $ config { T.cDelay = d }
    where d = maybe 0 id . readMaybe $ delay

configFormat :: String -> T.Config -> T.ErrMonad T.Config
configFormat arg config = maybe err go . C.readFormat $ arg
    where go x = pure $ config { T.cFormat = Just x }
          err  = throwError $ "Unrecognized format " <> arg
