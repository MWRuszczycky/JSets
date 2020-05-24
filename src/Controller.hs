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
import           Data.Text                    ( Text                   )
import           System.Directory             ( getHomeDirectory       )
import           System.Environment           ( getArgs                )
import           Text.Read                    ( readMaybe              )
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
configure = initConfig >>= byCommandLine >>= byFile

initConfig :: T.ErrMonad T.Config
initConfig = do
    hmPath <- liftIO getHomeDirectory
    pure T.Config { T.cOutputPath = Nothing
                  , T.cJsetKey    = Nothing
                  , T.cHelp       = False
                  , T.cRefPath    = hmPath <> "/.config/jsets/config"
                  , T.cReferences = []
                  , T.cToCStyle   = T.Propose
                  , T.cShowVer    = False
                  }

byCommandLine :: T.Config -> T.ErrMonad ([String], T.Config)
byCommandLine config = do
    args <- liftIO getArgs
    case Opt.getOpt Opt.Permute options args of
         (fs, cs, [] ) -> foldM (flip ($)) config fs >>= pure . ( (,) cs )
         (_,  _ , err) -> throwError . intercalate "\n" $ err

byFile :: ([String], T.Config) -> T.ErrMonad ([String], T.Config)
byFile (cmds, config) = do
    content <- C.readFileErr . T.cRefPath $ config
    case P.parseConfig content of
         Left  pErr -> configError config pErr
         Right xs   -> readConfig xs config >>= pure . (,) cmds

readConfig :: [[(Text, Text)]] -> T.Config -> T.ErrMonad T.Config
readConfig []     config = pure config
readConfig (_:xs) config = do
    case P.readRefs xs of
         Left err -> configError config err
         Right rs -> pure $ config { T.cReferences = rs }

configError :: T.Config -> T.ErrString -> T.ErrMonad a
configError config err = throwError msg
    where msg = "Unable to parse configuration file "
                <> T.cRefPath config <> "\n" <> err

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.ErrMonad T.Config) ]
options =
    [ Opt.Option "" [ "refs" ]
      ( Opt.ReqArg ( \ arg s -> pure s { T.cRefPath = arg } ) "PATH" )
      "Reset the references configuration path to PATH."

    , Opt.Option "h" [ "help" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cHelp = True } ) )
      "Display help information."

    , Opt.Option "k" [ "key" ]
      ( Opt.ReqArg configKey "KEY" )
      "Set the journal set key to KEY (positive integer)"

    , Opt.Option "o" [ "output" ]
      ( Opt.ReqArg ( \ arg s -> pure $ s { T.cOutputPath = Just arg } ) "PATH" )
      "Set the output filepath to PATH."

    , Opt.Option "r" [ "rank" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cToCStyle = T.Rank } ) )
      "Use the 'rank' style for html tables of contents."

    , Opt.Option "s" [ "select" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cToCStyle = T.Select } ) )
      "Use the 'select' style for html tables of contents."

    , Opt.Option "v" [ "version" ]
      ( Opt.NoArg ( \ s -> pure $ s { T.cShowVer = True } ) )
      "Just show the version number and quit."
    ]

configKey :: String -> T.Config -> T.ErrMonad T.Config
configKey key config
    | n < 1     = throwError $ "Key must be a positive integer."
    | otherwise = pure $ config { T.cJsetKey = Just n }
    where n = maybe 0 id . readMaybe $ key
