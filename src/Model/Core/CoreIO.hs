{-# LANGUAGE OverloadedStrings #-}

module Model.Core.CoreIO
    ( putTxtMIO
    , putTxtLnMIO
    , putStrMIO
    , putStrLnMIO
    , timeIt
    , writeFileErr
    , readFileErr
    , logFileErr
    , WebRequest
    , webRequest
    , webRequestIn
    ) where

import qualified Data.Text.IO           as Tx
import qualified Model.Core.Types       as T
import qualified Model.Core.Dates       as D
import qualified Network.Wreq           as Wreq
import qualified Network.Wreq.Session   as WreqS
import qualified Data.ByteString.Lazy   as BSL
import           Control.Monad.IO.Class          ( MonadIO, liftIO   )
import           System.IO                       ( hFlush, stdout    )
import           Data.Text                       ( Text              )
import           Data.Text.Encoding              ( decodeUtf8        )
import           Data.ByteString.Lazy            ( toStrict          )
import           Control.Monad.Except            ( ExceptT (..)      )
import           Lens.Micro                      ( (^.)              )
import           Control.Exception               ( IOException
                                                 , SomeException
                                                 , displayException
                                                 , catch
                                                 )

-- =============================================================== --
-- A little cleaner string and text printing

putTxtMIO :: MonadIO m => Text -> m ()
putTxtMIO t = do
    liftIO . Tx.putStr $ t
    liftIO . hFlush $ stdout

putTxtLnMIO :: MonadIO m => Text -> m ()
putTxtLnMIO = liftIO . Tx.putStrLn

putStrMIO :: MonadIO m => String -> m ()
putStrMIO s = do
     liftIO . putStr $ s
     liftIO . hFlush $ stdout

putStrLnMIO :: MonadIO m => String -> m ()
putStrLnMIO = liftIO . putStrLn

-- =============================================================== --
-- Timed IO processes

timeIt :: MonadIO m => m a -> m ( Integer, a )
-- ^Time how long a MonadIO transformer action takes.
timeIt axn = do
    start  <- liftIO D.readClock
    result <- axn
    delta  <- liftIO . D.deltaClock $ start
    pure (delta, result)

-- =============================================================== --
-- File IO management

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

logFileErr :: FilePath -> Text -> T.ErrMonad ()
-- ^Appends text to log a file.
logFileErr p t = ExceptT $ catch ( pure <$> Tx.appendFile p t ) hndlErr
    where hndlErr :: IOException -> IO ( Either T.ErrString () )
          hndlErr = pure . Left . (msg <>) . displayException
          msg     = "Unable to append to file: " <> p <> ". Details:\n"

-- =============================================================== --
-- Internet IO management

type WebRequest = Wreq.Options -> String -> T.ErrMonad Text

-- For this to work, the GHC.IO.Encoding should be set to utf8 in the
-- Main application source.

webRequest :: Wreq.Options -> String -> T.ErrMonad Text
-- ^Make a web request using the provided options and address.
webRequest os ad = ExceptT $ catch (readResponse <$> Wreq.getWith os ad) hndlErr
    where hndlErr :: SomeException -> IO ( Either T.ErrString Text)
          hndlErr = pure .Left . (msg <>) . displayException
          msg     = "Unable to make http request at: " <> ad <> ". Details:\n"

webRequestIn :: WreqS.Session -> Wreq.Options -> String -> T.ErrMonad Text
-- ^Make a web request using the provided options and address.
webRequestIn s os ad = ExceptT run
    where run     = catch (readResponse <$> WreqS.getWith os s ad) hndlErr
          hndlErr :: SomeException -> IO ( Either T.ErrString Text)
          hndlErr = pure .Left . (msg <>) . displayException
          msg     = "Unable to make http request at: " <> ad <> ". Details:\n"

readResponse :: Wreq.Response BSL.ByteString -> Either String Text
-- ^Read a web resopnse and check for errors.
readResponse resp
    | code == 200 = pure . decodeUtf8 . toStrict $ resp ^. Wreq.responseBody
    | otherwise   = Left $ "Cannot access, error code: " ++ show code
    where code = resp ^. Wreq.responseStatus . Wreq.statusCode
