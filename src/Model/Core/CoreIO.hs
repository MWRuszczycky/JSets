{-# LANGUAGE OverloadedStrings #-}

module Model.Core.CoreIO
    ( writeFileErr
    , readFileErr
    ) where

import qualified Data.Text.IO           as Tx
import qualified Model.Core.Types       as T
import           Data.Text                      ( Text                )
import           Control.Monad.Except           ( ExceptT (..)        )
import           Control.Exception              ( IOException
                                                , displayException
                                                , catch
                                                )

writeFileErr :: FilePath -> Text -> T.ErrMonad ()
-- ^writeFile adapted to the ErrMonad.
writeFileErr p t = ExceptT $ catch ( pure <$> Tx.writeFile p t ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = pure . Left . (msg <>) . displayException
          msg     = "Unable to write to file: " <> p <> ". Details:\n"

readFileErr :: FilePath -> T.ErrMonad Text
-- ^readFile adapted to the ErrMonad
readFileErr p = ExceptT $ catch ( pure <$> Tx.readFile p ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString Text )
          hndlErr = pure . Left . (msg <>) . displayException
          msg     = "Unable to read file: " <> p <> ". Details:\n"
