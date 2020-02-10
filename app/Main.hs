{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller           ( controller )
import Control.Monad.Except ( runExceptT )

main :: IO ()
main = do
    result <- runExceptT controller
    case result of
         Left err  -> putStrLn err
         _         -> pure ()
