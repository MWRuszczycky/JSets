{-# LANGUAGE OverloadedStrings #-}

module View.Core
    ( authorsToTxt
    ) where

import qualified Data.Text            as Tx
import qualified Model.Core.Types     as T
import           Data.Text                  ( Text )

authorsToTxt :: T.Citation -> Text
authorsToTxt c
    | null xs   = "No authors listed"
    | otherwise = Tx.intercalate ", " $ xs
    where xs = T.authors c
