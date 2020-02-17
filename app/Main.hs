{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GHC.IO.Encoding                     as GHC
import           Control.Monad.Except ( runExceptT )
import           Control.Monad.Reader ( runReaderT )
import           System.Environment   ( getArgs    )
import           Controller           ( controller
                                      , configure
                                      , finish     )

main :: IO ()
main = do
    GHC.setLocaleEncoding GHC.utf8
    etConfig <- configure <$> getArgs
    case etConfig of
         Left err -> putStrLn $ err <> "Try option '-h' or '--help' for usage."
         Right c  -> runExceptT (runReaderT controller c) >>= finish
