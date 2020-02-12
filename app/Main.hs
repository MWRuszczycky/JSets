{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except ( runExceptT )
import System.Environment   ( getArgs    )
import Controller           ( controller
                            , configure
                            , finish     )

main :: IO ()
main = do
    etSU <- configure <$> getArgs
    case etSU of
         Left err -> putStrLn $ err <> "Try option '-h' or '--help' for usage."
         Right su -> runExceptT (controller su) >>= finish
