{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Model.Core.CoreTH
    ( embedFile
    ) where

import qualified Language.Haskell.TH as TH

embedFile :: FilePath -> TH.Q TH.Exp
embedFile fp = do
    content <- TH.runIO . readFile $ fp
    [| content |]
