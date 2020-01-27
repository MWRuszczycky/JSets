{-# LANGUAGE OverloadedStrings #-}

import qualified JournalsTests    as JT
import qualified CoreTests        as CT

main :: IO ()
main = do
    JT.spec
    CT.spec
