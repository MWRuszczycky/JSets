{-# LANGUAGE OverloadedStrings #-}

import qualified ModelSpec.Journals as J
import qualified ModelSpec.Core     as C
import qualified ModelSpec.Parsers  as P

main :: IO ()
main = do
    J.spec
    C.spec
    P.spec
