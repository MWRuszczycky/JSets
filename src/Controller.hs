{-# LANGUAGE OverloadedStrings #-}

module Controller
    ( controller
    ) where

import qualified Data.Text.IO              as Tx
import qualified Data.Text                 as Tx
import qualified Data.Map.Strict           as Map
import qualified Model.Core.Types          as T
import qualified Model.Core.References     as R
import qualified Model.Journals            as J
import qualified CoreIO                    as CIO
import qualified Viewer                    as V
import           Data.Text                          ( Text   )
import           Control.Monad.Except               ( liftIO )

controller :: T.ErrMonad ()
controller = do
    let Just jset1 = Map.lookup (2019,1) . J.yearly26Sets 2019 $ R.issueRefs
    jsetToMkd ((2019,1), jset1)

-- ================================================================ --
-- Commands

jsetToMkd :: T.JournalSet -> T.ErrMonad ()
jsetToMkd (_,xs) = mapM CIO.downloadToC xs >>= liftIO . Tx.writeFile fn . go
    where fn = "dev/tocs.mkd"
          go = Tx.unlines . map V.tocToMkd
