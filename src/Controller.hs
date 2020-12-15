{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( runApp
    , configureApp
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
import           Commands                     ( runCommand, commands )
import           Control.Monad                ( foldM, when          )
import           Control.Monad.Except         ( throwError           )
import           Control.Monad.Reader         ( runReaderT, liftIO   )
import           Data.List                    ( intercalate          )
import           Data.Text                    ( Text, pack           )
import           System.Directory             ( getHomeDirectory
                                              , doesFileExist        )
import           System.IO                    ( stdout
                                              , hIsTerminalDevice    )
import           System.Environment           ( getArgs              )
import           Text.Read                    ( readMaybe            )

-- =============================================================== --
-- Main control point and routers

runApp :: T.Config -> T.ErrMonad ()
runApp config
    | T.cHelp config    = liftIO . Tx.putStrLn . H.summary commands $ options
    | T.cShowVer config = liftIO . Tx.putStrLn $ H.version
    | otherwise         = runReaderT ( runCommand args ) config
    where args = T.cArguments config

-- =============================================================== --
-- Configuration

configureApp :: T.ErrMonad T.Config
configureApp = do
    (ws,config) <- initialize >>= configure
    -- Configure the terminal if necessary
    when (T.cStdOutIsTerm  config) . liftIO . putStr $ "\ESC[0m"
    when (not . T.cTerse $ config) . mapM_ warn $ ws
    pure config

---------------------------------------------------------------------
-- Configuration phases

initialize :: T.ErrMonad T.Config
-- ^Start with the default configuration and try to find the default
-- configuration file.
initialize = do
    path   <- ( <> "/.config/jsets/config" ) <$> liftIO getHomeDirectory
    exists <- liftIO . doesFileExist $ path
    pure $ T.defaultConfig
           { T.cConfigPath = if exists then Just path else Nothing }

configure :: T.Config -> T.ErrMonad ([Text], T.Config)
-- ^Configuration needs to follow a certain precedence:
-- 1. Initial configurators from the command line get applied first,
--    because the resulting configuration can influence subsequent
--    configuration steps. For example, setting of the configuration
--    file path is an initial step that ensures any configuration file
--    set at the command line is used instead of the default
--    configuration file.
-- 2. Initial configurators from the configuration file are then
--    applied for the same reason as above.
-- 3. General configurators from the file are then applied.
-- 4. General command line configurators are then applied. This
--    ensures that command line parameters take precedence over
--    any parameters set in a configuration file.
-- 5. Finally, configure any parameters not available to the user,
--    and return any warnings encountered.
configure config = do
    -- Initialize the configuration from the command line
    cliSteps <- commandLineConfigSteps
    let (cliInits, cliGenerals) = getConfigurators cliSteps
        cliWarnings             = getWarnings      cliSteps
    configInitByCLI <- foldM (flip ($)) config cliInits

    -- Apply all the remaining configuration steps in the proper order
    fileSteps <- fileConfigSteps configInitByCLI
    let (fileInits, fileGenerals) = getConfigurators fileSteps
        fileWarnings              = getWarnings      fileSteps
        remainingSteps            = fileInits <> fileGenerals <> cliGenerals
    configFinal <- foldM (flip ($)) configInitByCLI $ remainingSteps

    -- Finalize the configuration
    today  <- liftIO Cd.today
    isTerm <- liftIO . hIsTerminalDevice $ stdout
    pure . (,) (cliWarnings <> fileWarnings) $ configFinal
        { T.cDate         = today
        , T.cStdOutIsTerm = isTerm
        , T.cFormat       = Nothing -- TODO fix this
        }

---------------------------------------------------------------------
-- Configuration helpers

getConfigurators :: [T.ConfigStep] -> ([T.Configurator],[T.Configurator])
-- ^Parse configuration steps into steps that should be applied first
-- versus those that should be applied later.
getConfigurators = foldr go ([],[])
    where go (T.ConfigInit    f) (fs,gs) = (f:fs,gs)
          go (T.ConfigGen     g) (fs,gs) = (fs,g:gs)
          go _                   (fs,gs) = (fs,gs)

getWarnings :: [T.ConfigStep] -> [Text]
-- ^Parse configuration steps to get all the warnings.
getWarnings = foldr go []
    where go (T.ConfigWarn w) ws = w:ws
          go _                ws = ws

warn :: Text -> T.ErrMonad ()
-- ^Display a configuration warning.
warn w = liftIO . Tx.putStrLn $ "Warning: " <> w

commandLineConfigSteps :: T.ErrMonad [T.ConfigStep]
-- ^Get all the configuration steps from the command line interface.
commandLineConfigSteps = do
    args <- liftIO getArgs
    case Opt.getOpt Opt.Permute options args of
         (fs, xs, [] ) -> pure $ T.ConfigInit (configArguments xs) : fs
         (_ , _ , err) -> throwError . intercalate "\n" $ err

fileConfigSteps :: T.Config -> T.ErrMonad [T.ConfigStep]
-- ^Get all the configuration steps from the configuration file
-- specified in a properly initialized configuration.
fileConfigSteps config = do
    let path = T.cConfigPath config
    content <- maybe (pure Tx.empty) C.readFileErr $ path
    case P.parseConfig content >>= P.readConfig of
         Right steps -> pure steps
         Left  err   -> throwError $ "Error: Unable to configure JSets! \n"
                                     <> err

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr T.ConfigStep ]
options =
    [
    -- Parameters

      Opt.Option "c" [ "config" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigInit $ \ c -> pure $ c { T.cConfigPath = Just x } )
          "PATH"
      ) "Use the configuration file at path PATH."

    , Opt.Option "" [ "user" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ \ c -> pure $ c { T.cUser = Just . pack $ x } )
          "USER"
      ) "Override the configured user name with USER."

    , Opt.Option "" [ "email" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ \ c -> pure $ c { T.cEmail = Just . pack $ x } )
          "EMAIL"
      ) "Override the configured email with EMAIL."

    , Opt.Option "k" [ "key" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configKey x )
          "KEY"
      ) "Set the journal set key to KEY (positive integer)."

    , Opt.Option "o" [ "output" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ \ c -> pure $ c { T.cOutputPath = Just x } )
          "PATH"
      ) "Set the output filepath to PATH."

    , Opt.Option "" [ "delay" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configDelay x )
          "SEC"
      ) "Delay in whole seconds between PubMed requests."

    , Opt.Option "" [ "fmt" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configFormat x )
          "FMT"
      ) "Set the output format to FMT overriding file extension."

    -- Flags

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cHelp = True } )
      ) "Display help information."

    , Opt.Option "v" [ "version" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cShowVer = True } )
      ) "Just show the version number and quit."

    , Opt.Option "" [ "match-details" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cMatchDetails = True } )
      ) "Provide detailed match output."

    , Opt.Option "" [ "no-sort" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cSortJSets = False } )
      ) "Do not sort journal set issues."

    , Opt.Option "t" [ "terse" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cTerse = True } )
      ) "Do not display messages & accept all defaults."
    ]

configKey :: String -> T.Configurator
configKey key config
    | n < 1     = throwError $ "Key must be a positive integer."
    | otherwise = pure $ config { T.cJSetKey = Just n }
    where n = maybe 0 id . readMaybe $ key

configDelay :: String -> T.Configurator
configDelay delay config
    | d < 1     = throwError $ "Delay time must be a positive integer."
    | otherwise = pure $ config { T.cDelay = d }
    where d = maybe 0 id . readMaybe $ delay

configFormat :: String -> T.Configurator
configFormat arg config = maybe err go . C.readFormat $ arg
    where go x = pure $ config { T.cFormat = Just x }
          err  = throwError $ "Unrecognized format " <> arg

configArguments :: [String] -> T.Configurator
configArguments args config = pure $ config { T.cArguments = args }
