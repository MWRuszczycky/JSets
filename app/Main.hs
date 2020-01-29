{-# LANGUAGE OverloadedStrings #-}

module Main where

import Controller ( controller, parseTest )

main :: IO ()
main = parseTest --controller
