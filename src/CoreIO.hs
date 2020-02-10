{-# LANGUAGE OverloadedStrings #-}
module CoreIO
    ( downloadToC
    , downloadToCTxt
    ) where

import qualified Model.Core.Types       as T
import qualified Network.Wreq           as Wreq
import qualified Model.Journals         as J
import qualified Model.Parsers.PubMed   as P
import           Data.Text                      ( Text )
import           Control.Monad.Except           ( ExceptT (..)
                                                , liftEither    )

---------------------------------------------------------------------
-- Downloading table of contents

downloadToC :: T.Issue -> T.ErrMonad T.TableOfContents
downloadToC x = downloadToCTxt x >>= liftEither . P.parseToC x

downloadToCTxt :: T.Issue -> T.ErrMonad Text
downloadToCTxt x = ExceptT $ J.readResponse <$> Wreq.getWith opts addr
    where addr = "https://www.ncbi.nlm.nih.gov/pubmed"
          opts = J.tocQuery x
