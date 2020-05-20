{-# LANGUAGE OverloadedStrings #-}

module Model.Parsers.Basic
    ( JsonVal   (..)
    , CSV
    , parseCSV
    , parseJSON
    ) where

import qualified Data.Text            as Tx
import qualified Data.Attoparsec.Text as At
import           Data.Text                   ( Text  )
import           Data.Bifunctor              ( bimap )
import           Control.Applicative         ( many  )

-- =============================================================== --
-- Types

data JsonVal =
      JStr   Text
    | JNum   Text
    | JBool  Bool
    | JObj   [ (Text, JsonVal) ]
    | JArray [ JsonVal ]
      deriving ( Show )

type CSV = [[Text]]

-- =============================================================== --
-- Exported parsers

parseCSV :: Text -> Either String CSV
parseCSV = bimap err id . At.parseOnly csv
    where err = (++) "Cannot parse CSV: "

-- =============================================================== --
-- General parsers

quoted :: At.Parser Text
quoted = At.char '\"' *> go
    where go = do x <- At.anyChar
                  case x of
                       '\\' -> Tx.cons <$> At.anyChar <*> go
                       '\"' -> pure Tx.empty
                       _    -> Tx.cons x <$> go

-- =============================================================== --
-- CSV Parser components

csv :: At.Parser CSV
csv = many csvLine <* At.endOfInput

csvLine :: At.Parser [Text]
csvLine = At.sepBy csvCell (At.char ',') <* At.endOfLine

csvCell :: At.Parser Text
csvCell = do
    x <- At.peekChar'
    case x of
         '\"' -> quoted
         _    -> At.takeTill $ flip elem [',', '\n', '\r']

-- =============================================================== --
-- JSON parser components

parseJSON :: Text -> Either String JsonVal
parseJSON = bimap err id . At.parseOnly json
    where err = (++) "Cannot parse JSON: "

json :: At.Parser JsonVal
json = undefined
