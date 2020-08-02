{-# LANGUAGE OverloadedStrings #-}

import qualified ModelSpec.Journals as J
import qualified ModelSpec.Core     as C

main :: IO ()
main = do
    J.spec
    C.spec
