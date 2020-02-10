{-# LANGUAGE OverloadedStrings #-}
module CoreIO
    ( downloadToC
    , downloadToCTxt
    ) where

import qualified Model.Core.Types     as T
import qualified Network.Wreq         as Wreq
import qualified Model.Journals       as J
import qualified Model.Parsers.PubMed as P
import           Data.Text                      ( Text )

---------------------------------------------------------------------
-- Downloading table of contents

downloadToC :: T.Issue -> IO (Either String T.TableOfContents)
downloadToC x = do
    result <- downloadToCTxt x
    case result of
         Left err  -> pure . Left $ "Cannot download ToC: " <> err
         Right toc -> pure . P.parseToC x $ toc

downloadToCTxt :: T.Issue -> IO (Either String Text)
downloadToCTxt x = Wreq.getWith opts address >>= pure . J.readResponse
    where address = "https://www.ncbi.nlm.nih.gov/pubmed"
          opts    = J.tocQuery x
