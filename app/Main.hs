{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except ( runExceptT )
import System.Environment   ( getArgs    )
import Controller           ( controller
                            , getSetup
                            , finish     )

main :: IO ()
main = do
    etSU <- getSetup <$> getArgs
    case etSU of
         Left err -> putStrLn $ err <> "Try option '-h' or '--help' for usage."
         Right su -> runExceptT (controller su) >>= finish
