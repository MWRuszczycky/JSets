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
import           Control.Monad.Reader               ( asks                )
import           Text.Read                          ( readMaybe           )

-- =============================================================== --
-- Main control point and routers

controller :: T.AppMonad Tx.Text
controller = do
    runHelp <- asks T.cHelp
    if runHelp
       then pure "Print help."
       else asks T.cCmds >>= route

route :: [String] -> T.AppMonad Tx.Text
route ("toc":_) = C.writeTocsToMkd
route (x:_)     = maybe (badCommand x) C.writeJsetsByYear . readMaybe $ x
route ([])      = pure "Nothing to do."

badCommand :: String -> T.AppMonad Tx.Text
badCommand = throwError . (<>) "Unrecognized command "

finish :: Either String Tx.Text -> IO ()
finish (Right msg) = Tx.putStrLn msg
finish (Left err)  = putStr $ unlines [ err, msg ]
    where msg = "Try option '-h' or '--help' for usage."

-- =============================================================== --
-- Options

options :: [ Opt.OptDescr (T.Config -> T.Config) ]
options =
    [ Opt.Option "o" [ "output-path" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cOutputPath = Just arg } ) "Path" )
      "Set the output-directory to DIR."

    , Opt.Option "f" [ "file", "input-path" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cInputPath = Just arg } ) "PATH" )
      "Set filepath for the journal sets to PATH."

    , Opt.Option "h" [ "help", "info", "information" ]
      ( Opt.NoArg ( \ s -> s { T.cHelp = True } ) )
      "Display help information."

    , Opt.Option "k" [ "jset", "key", "journal-set" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cJsetKey = Just arg } ) "KEY" )
      "Set the journal set key to KEY"

    , Opt.Option "y" [ "year" ]
      ( Opt.ReqArg ( \ arg s -> s { T.cJsetsYear = Just arg } ) "YEAR" )
      "Set the year for the journal sets to YEAR."
    ]

configure :: [String] -> Either T.ErrString T.Config
configure xs =
    case Opt.getOpt Opt.Permute options xs of
         (fs,cs,[] ) -> pure . foldl' (flip ($)) (def { T.cCmds = cs }) $ fs
         (_ ,_ ,err) -> Left . intercalate "\n" $ err
