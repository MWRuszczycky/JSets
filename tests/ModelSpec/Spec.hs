{-# LANGUAGE OverloadedStrings #-}

import qualified ModelSpec.Core     as C
import qualified ModelSpec.Journals as J
import qualified ModelSpec.Matching as Mt

main :: IO ()
main = do
    C.spec
    J.spec
    Mt.spec
