{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runApp
    , configure
    ) where

import qualified System.Console.GetOpt as Opt
import qualified Data.Text.IO          as Tx
import qualified Model.Core.Types      as T
import qualified Model.Core.CoreIO     as C
import qualified Model.Parsers.Config  as P
import qualified View.Help             as H
import           System.Directory             ( getHomeDirectory       )
import           System.Environment           ( getArgs                )
import           Text.Read                    ( readMaybe              )
import           Data.Text                    ( pack                   )
import           Data.List                    ( intercalate            )
import           Control.Monad                ( foldM                  )
import           Control.Monad.Except         ( throwError             )
import           Control.Monad.Reader         ( runReaderT, liftIO     )
import           Commands                     ( runCommands, commands  )

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

configError :: T.Config -> T.ErrString -> T.ErrMonad a
configError config err = throwError msg
    where msg = "Unable to parse configuration file "
                <> T.cRefPath config <> "\n" <> err

initConfig :: T.ErrMonad T.Config
initConfig = do
    hmPath <- liftIO getHomeDirectory
    pure $ T.defaultConfig { T.cRefPath = hmPath <> "/.config/jsets/config" }

configFromCommandLine :: T.Config -> T.ErrMonad ([String], T.Config)
configFromCommandLine config = do
    args <- liftIO getArgs
    case Opt.getOpt Opt.Permute options args of
         (fs, cs, [] ) -> foldM (flip ($)) config fs >>= pure . ( (,) cs )
         (_,  _ , err) -> throwError . intercalate "\n" $ err

configFromFile :: ([String], T.Config) -> T.ErrMonad ([String], T.Config)
configFromFile (cmds, config) = do
    content <- C.readFileErr . T.cRefPath $ config
    case P.parseConfig content >>= P.readConfig config of
         Left  pErr    -> configError config pErr
         Right config' -> pure ( cmds , config' )

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.ErrMonad T.Config) ]
options =
    [ Opt.Option "" [ "email" ]
      ( Opt.ReqArg ( \ x s -> pure s { T.cEmail = Just . pack $ x } ) "EMAIL" )
      "Override the configured email with EMAIL."

    , Opt.Option "" [ "nickname" ]
      ( Opt.ReqArg ( \ x s -> pure s { T.cNick = Just . pack $ x } ) "NICK" )
      "Override the configured nickname with NICK."

    , Opt.Option "" [ "refs" ]
      ( Opt.ReqArg ( \ x s -> pure s { T.cRefPath = x } ) "PATH" )
      "Reset the references configuration path to PATH."

    , Opt.Option "" [ "user" ]
      ( Opt.ReqArg ( \ x s -> pure s { T.cUser = Just . pack $ x } ) "USER" )
      "Override the configured user name with USER."

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cHelp = True } ) )
      "Display help information."

    , Opt.Option "i" [ "instruct" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cInstrToc = True } ) )
      "Show instructions in html ToC output."

    , Opt.Option "k" [ "key" ]
      ( Opt.ReqArg configKey "KEY" )
      "Set the journal set key to KEY (positive integer)"

    , Opt.Option "o" [ "output" ]
      ( Opt.ReqArg ( \ x s -> pure $ s { T.cOutputPath = Just x } ) "PATH" )
      "Set the output filepath to PATH."

    , Opt.Option "v" [ "version" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cShowVer = True } ) )
      "Just show the version number and quit."
    ]

configKey :: String -> T.Config -> T.ErrMonad T.Config
configKey key config
    | n < 1     = throwError $ "Key must be a positive integer."
    | otherwise = pure $ config { T.cJSetKey = Just n }
    where n = maybe 0 id . readMaybe $ key
