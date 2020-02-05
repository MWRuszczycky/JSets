{-# LANGUAGE OverloadedStrings #-}

import qualified JournalsTests    as JT
import qualified CoreTests        as CT
import qualified ParserTests      as PT

main :: IO ()
main = do
    JT.spec
    CT.spec
    PT.spec
