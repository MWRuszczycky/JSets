{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GHC.IO.Encoding                     as GHC
import           Control.Monad.Except ( runExceptT )
import           Controller           ( configure
                                      , runApp     )

main :: IO ()
main = do
    GHC.setLocaleEncoding GHC.utf8
    result <- runExceptT $ configure >>= runApp
    case result of
         Left err -> putStrLn $ err <> "Try option '-h' or '--help' for usage."
         Right _  -> pure ()
