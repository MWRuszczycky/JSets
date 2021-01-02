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
import           Data.Char                    ( isDigit              )
import           Data.List                    ( nub, intercalate     )
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
    checkForDuplicateReferences config
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
        }

---------------------------------------------------------------------
-- Configuration helpers

getConfigurators :: [T.ConfigStep] -> ([T.Configurator],[T.Configurator])
-- ^Parse configuration steps into steps that should be applied first
-- versus those that should be applied later.
getConfigurators = foldr go ([],[])
    where go (T.ConfigInit f) (fs,gs) = (f:fs,gs)
          go (T.ConfigGen  g) (fs,gs) = (fs,g:gs)
          go _                (fs,gs) = (fs,gs)

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
    case P.parseConfig content of
         Right steps -> pure steps
         Left  err   -> throwError $ "Error: Unable to configure JSets! \n"
                                     <> err

checkForDuplicateReferences :: T.Config -> T.ErrMonad ()
-- ^Journal entries are all keyed by their abbreviations. So, the
-- journal abbreviations must be unique. However, this function also
-- checks to make sure the journal names are also unique.
checkForDuplicateReferences config
    | gNames && gAbbrs = pure ()
    | gNames           = throwError abbrErr
    | otherwise        = throwError nameErr
    where gNames  = xs == nub xs
          gAbbrs  = ys == nub ys
          xs      = map (T.name . T.journal) . T.cReferences $ config
          ys      = map (T.abbr . T.journal) . T.cReferences $ config
          abbrErr = "References have repeated journal abbreviations!"
          nameErr = "References have repeated journal names!"

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

    , Opt.Option "o" [ "output" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigInit $ configOutputPath x )
          "PATH"
      ) "Set the output filepath to PATH."

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

    , Opt.Option "" [ "delay" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configDelay x )
          "SEC"
      ) "Delay in whole seconds between PubMed requests."

    , Opt.Option "" [ "max-results" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configMaxResults x )
          "MAX"
      ) "Maximum number of PubMed results to return (default: 200)."

    , Opt.Option "" [ "fmt" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configFormat x )
          "FMT"
      ) "Set the output format to FMT overriding file extension."

    , Opt.Option "" [ "author" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configAddToQuery . T.AuthorQry . Tx.pack $ x )
          "AUTHOR"
      ) "Set the author field for PubMed queries"

    , Opt.Option "" [ "title" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configAddToQuery . T.TitleQry . Tx.pack $ x )
          "TITLE"
      ) "Set the title field for PubMed queries"

    , Opt.Option "" [ "page" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configPageQuery x )
          "PAGE"
      ) "Set the page field for PubMed queries"

    , Opt.Option "" [ "doi" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configAddToQuery . T.DOIQry . Tx.pack $ x )
          "DOI"
      ) "Set the doi field for PubMed queries"

    , Opt.Option "" [ "journal" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configAddToQuery . T.JournalQry . Tx.pack $ x )
          "JOURNAL"
      ) "Set the journal field for PubMed queries"

    , Opt.Option "" [ "pmid" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configAddToQuery . T.PMIDQry . Tx.pack $ x )
          "PMID"
      ) "Set the pmid field for PubMed queries"

    , Opt.Option "" [ "year" ]
      ( Opt.ReqArg
          ( \ x -> T.ConfigGen $ configIntQuery T.YearQry x )
          "YEAR"
      ) "Set the year field for PubMed queries"

     , Opt.Option "" [ "issue" ]
       ( Opt.ReqArg
           ( \ x -> T.ConfigGen $ configIntQuery T.NumberQry x )
           "ISSUE"
       ) "Set the issue number field for PubMed queries"

     , Opt.Option "" [ "volume" ]
       ( Opt.ReqArg
           ( \ x -> T.ConfigGen $ configIntQuery T.VolumeQry x )
           "VOLUME"
       ) "Set the volume number field for PubMed queries"

    -- Flags

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cHelp = True } )
      ) "Display help information."

    , Opt.Option "v" [ "version" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cShowVer = True } )
      ) "Just show the version number and quit."

    , Opt.Option "" [ "by-date" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cYearlyByDate = True } )
      ) "Order issues by date when grouping with <year> command."

    , Opt.Option "" [ "match-details" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cMatchDetails = True } )
      ) "Provide detailed match output."

    , Opt.Option "" [ "no-sort" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cSortJSets = False } )
      ) "Do not sort journal set issues."

    , Opt.Option "" [ "pmids-only" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cOnlyPMIDs = True } )
      ) "Return only PubMed IDs when querying PubMed."

    , Opt.Option "t" [ "terse" ]
      ( Opt.NoArg
          ( T.ConfigGen $ \ c -> pure $ c { T.cTerse = True } )
      ) "Do not display messages & accept all defaults."
    ]

configAddToQuery :: T.QueryTerm -> T.Configurator
configAddToQuery q config = pure $ config { T.cQuery = q:qs}
    where qs = T.cQuery config

configIntQuery :: (Int -> T.QueryTerm) -> String -> T.Configurator
configIntQuery q x config = go x >>= flip configAddToQuery config . q
    where go x = maybe err chk . readMaybe $ x
          err  = throwError $ "Argument " <> x <> " is not an unsigned integer!"
          chk n | n < 0     = err
                | otherwise = pure n

configPageQuery :: String -> T.Configurator
configPageQuery xs config = go pgno >>= flip configAddToQuery config
    where (rd,rp) = span isDigit . reverse $ xs
          pgno    = T.PageNo (reverse rp) <$> (readMaybe . reverse $ rd)
          go      = maybe err (pure . T.PageQry)
          err     = throwError $ "Invalid page number " <> xs <> "!"

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

configMaxResults :: String -> T.Configurator
configMaxResults maxresults config
    | n < 1     = throwError $ "Maximum results must be a positive integer."
    | otherwise = pure $ config { T.cMaxResults = n }
    where n = maybe 0 id . readMaybe $ maxresults

configFormat :: String -> T.Configurator
-- ^This should be a part of a general configuration step so that it
-- takes precedence over the format set by the output path extension. 
configFormat arg config = maybe err go . C.readFormat $ arg
    where go x = pure $ config { T.cFormat = x }
          err  = throwError $ "Unrecognized format " <> arg

configArguments :: [String] -> T.Configurator
configArguments args config = pure $ config { T.cArguments = args }

configOutputPath :: FilePath -> T.Configurator
-- ^Configure both the output path as well as the format. This should
-- be an initial configuration step so that the format provided by
-- the --fmt option will take precedence if used.
configOutputPath fp config = pure $
    config { T.cOutputPath = Just fp
           , T.cFormat     = maybe (T.cFormat config) id
                             . C.readFormat . C.extension $ fp }
