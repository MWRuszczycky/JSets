{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GHC.IO.Encoding      as GHC
import           System.Exit                  ( die          )
import           Control.Monad.Except         ( runExceptT   )
import           Controller                   ( configureApp
                                              , runApp       )

main :: IO ()
main = do
    GHC.setLocaleEncoding GHC.utf8
    result <- runExceptT $ configureApp >>= runApp
    case result of
         Right _  -> pure ()
         Left err -> die $ err <> "\nTry '-h' or '--help' for usage."
