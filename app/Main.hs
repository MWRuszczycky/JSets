{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GHC.IO.Encoding      as GHC
import           Control.Monad.Except        ( runExceptT   )
import           Controller                  ( configureApp
                                             , runApp       )

main :: IO ()
main = do
    GHC.setLocaleEncoding GHC.utf8
    result <- runExceptT $ configureApp >>= runApp
    case result of
         Left err -> putStrLn $ err <> "\nTry '-h' or '--help' for usage."
         Right _  -> pure ()
